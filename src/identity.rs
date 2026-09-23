// 增量身份：编辑后重新解析时，把旧 AST 的节点身份（nid）映射到新 AST。
//
// 对齐规则（结构性，不依赖变量名/字面量值）：
// - 节点签名（kind + record 字段集合/field、variant 标签）相同才可能复用；
// - 子节点按“最小编辑距离（LCS）”对齐；
// - record 字段按 label 对齐；match arm 按位置；lambda 参数按模式对齐。
// 因此：变量改名、改数字、移动 let 顺序之外的局部编辑都不会引起身份漂移。
use crate::syntax::{Expr, Pat};

pub struct RemapResult {
    pub expr: Expr,
    pub max_id: u32,
    pub reused: u32,
    pub fresh: u32,
}

pub fn remap(new_tree: Expr, old: Option<&Expr>, old_max: u32) -> RemapResult {
    let mut a = Allocator { next: old_max + 1, reused: 0, fresh: 0 };
    let expr = remap_expr(new_tree, old, &mut a);
    RemapResult { expr, max_id: a.next.saturating_sub(1), reused: a.reused, fresh: a.fresh }
}

struct Allocator {
    next: u32,
    reused: u32,
    fresh: u32,
}
impl Allocator {
    fn reuse(&mut self, old_nid: u32) -> u32 {
        self.reused += 1;
        old_nid
    }
    fn fresh_id(&mut self) -> u32 {
        let id = self.next;
        self.next += 1;
        self.fresh += 1;
        id
    }
}

fn remap_expr(
    new: Expr,
    old: Option<&Expr>,
    al: &mut Allocator,
) -> Expr {
    let same_sig = old.map(|o| o.signature() == new.signature()).unwrap_or(false);
    if !same_sig {
        return rebuild_fresh(new, al);
    }
    let old_e = old.unwrap();
    let id = al.reuse(old_e.nid());
    match (new, old_e) {
        (Expr::Int { value, span, .. }, _) => Expr::Int { value, span, nid: id },
        (Expr::Bool { value, span, .. }, _) => Expr::Bool { value, span, nid: id },
        (Expr::Str { value, span, .. }, _) => Expr::Str { value, span, nid: id },
        // Var 同名与否都复用（改名不应漂移）
        (Expr::Var { name, span, .. }, _) => Expr::Var { name, span, nid: id },
        (
            Expr::Lam { param, body, span, .. },
            Expr::Lam {
                param: op,
                body: ob,
                ..
            },
        ) => Expr::Lam {
            param: remap_pat(param, Some(op), al),
            body: Box::new(remap_expr(*body, Some(ob), al)),
            span,
            nid: id,
        },
        (
            Expr::App { fun, arg, span, .. },
            Expr::App {
                fun: of,
                arg: oa,
                ..
            },
        ) => {
            let (fun_v, arg_v) = lcs_pair(*fun, of, *arg, oa, al);
            Expr::App { fun: Box::new(fun_v), arg: Box::new(arg_v), span, nid: id }
        }
        (
            Expr::Let { is_rec, name, value, body, span, .. },
            Expr::Let {
                name: on,
                value: ov,
                body: ob,
                ..
            },
        ) => Expr::Let {
            is_rec,
            name: remap_pat(name, Some(on), al),
            value: Box::new(remap_expr(*value, Some(ov), al)),
            body: Box::new(remap_expr(*body, Some(ob), al)),
            span,
            nid: id,
        },
        (
            Expr::If { cond, then_e, else_e, span, .. },
            Expr::If {
                cond: oc,
                then_e: ot,
                else_e: oe,
                ..
            },
        ) => Expr::If {
            cond: Box::new(remap_expr(*cond, Some(oc), al)),
            then_e: Box::new(remap_expr(*then_e, Some(ot), al)),
            else_e: Box::new(remap_expr(*else_e, Some(oe), al)),
            span,
            nid: id,
        },
        (
            Expr::Record { mut fields, span, .. },
            Expr::Record { fields: of, .. },
        ) => {
            let mut result = Vec::new();
            for (label, fe) in fields.drain(..) {
                let old_fe = of.iter().find(|(l, _)| l == &label).map(|(_, e)| e);
                result.push((
                    label,
                    remap_expr(fe, old_fe, al),
                ));
            }
            Expr::Record { fields: result, span, nid: id }
        }
        (
            Expr::Field { record, label, span, .. },
            Expr::Field { record: or, .. },
        ) => Expr::Field {
            record: Box::new(remap_expr(*record, Some(or), al)),
            label,
            span,
            nid: id,
        },
        (
            Expr::Variant { label, payload, span, .. },
            Expr::Variant { payload: op, .. },
        ) => Expr::Variant {
            label,
            payload: match (payload, op) {
                (Some(p), Some(o)) => Some(Box::new(remap_expr(*p, Some(o), al))),
                (Some(p), None) => Some(Box::new(rebuild_fresh(*p, al))),
                (None, _) => None,
            },
            span,
            nid: id,
        },
        (
            Expr::Match { scrutinee, mut arms, span, .. },
            Expr::Match {
                scrutinee: os,
                arms: oa,
                ..
            },
        ) => {
            let sc = remap_expr(*scrutinee, Some(os), al);
            let mut new_arms = Vec::new();
            for (i, (pat, body)) in arms.drain(..).enumerate() {
                let (op, ob) = oa.get(i).map(|(p, b)| (Some(p), Some(b))).unwrap_or((None, None));
                new_arms.push((
                    remap_pat(pat, op, al),
                    remap_expr(body, ob, al),
                ));
            }
            Expr::Match { scrutinee: Box::new(sc), arms: new_arms, span, nid: id }
        }
        // 签名相同但构造不同不会发生；保险起见整棵重建
        (other, _) => rebuild_fresh(other, al),
    }
}

fn remap_pat(
    new: Pat,
    old: Option<&Pat>,
    al: &mut Allocator,
) -> Pat {
    if old.map(|o| o.signature() == new.signature()).unwrap_or(false) {
        let id = al.reuse(old.unwrap().nid());
        match new {
            Pat::PVar { name, span, .. } => Pat::PVar { name, span, nid: id },
            Pat::PWild { span, .. } => Pat::PWild { span, nid: id },
        }
    } else {
        rebuild_pat_fresh(new, al)
    }
}

fn rebuild_fresh(e: Expr, al: &mut Allocator) -> Expr {
    let nid = al.fresh_id();
    match e {
        Expr::Int { value, span, .. } => Expr::Int { value, span, nid },
        Expr::Bool { value, span, .. } => Expr::Bool { value, span, nid },
        Expr::Str { value, span, .. } => Expr::Str { value, span, nid },
        Expr::Var { name, span, .. } => Expr::Var { name, span, nid },
        Expr::Lam { param, body, span, .. } => Expr::Lam {
            param: rebuild_pat_fresh(param, al),
            body: Box::new(rebuild_fresh(*body, al)),
            span,
            nid,
        },
        Expr::App { fun, arg, span, .. } => Expr::App {
            fun: Box::new(rebuild_fresh(*fun, al)),
            arg: Box::new(rebuild_fresh(*arg, al)),
            span,
            nid,
        },
        Expr::Let { is_rec, name, value, body, span, .. } => Expr::Let {
            is_rec,
            name: rebuild_pat_fresh(name, al),
            value: Box::new(rebuild_fresh(*value, al)),
            body: Box::new(rebuild_fresh(*body, al)),
            span,
            nid,
        },
        Expr::If { cond, then_e, else_e, span, .. } => Expr::If {
            cond: Box::new(rebuild_fresh(*cond, al)),
            then_e: Box::new(rebuild_fresh(*then_e, al)),
            else_e: Box::new(rebuild_fresh(*else_e, al)),
            span,
            nid,
        },
        Expr::Record { fields, span, .. } => Expr::Record {
            fields: fields
                .into_iter()
                .map(|(l, e)| (l, rebuild_fresh(e, al)))
                .collect(),
            span,
            nid,
        },
        Expr::Field { record, label, span, .. } => Expr::Field {
            record: Box::new(rebuild_fresh(*record, al)),
            label,
            span,
            nid,
        },
        Expr::Variant { label, payload, span, .. } => Expr::Variant {
            label,
            payload: payload.map(|p| Box::new(rebuild_fresh(*p, al))),
            span,
            nid,
        },
        Expr::Match { scrutinee, arms, span, .. } => Expr::Match {
            scrutinee: Box::new(rebuild_fresh(*scrutinee, al)),
            arms: arms
                .into_iter()
                .map(|(p, e)| (rebuild_pat_fresh(p, al), rebuild_fresh(e, al)))
                .collect(),
            span,
            nid,
        },
    }
}

fn rebuild_pat_fresh(p: Pat, al: &mut Allocator) -> Pat {
    let nid = al.fresh_id();
    match p {
        Pat::PVar { name, span, .. } => Pat::PVar { name, span, nid },
        Pat::PWild { span, .. } => Pat::PWild { span, nid },
    }
}

// App 的两个子项在交换（管道重写等）情况下做最优对齐。
fn lcs_pair(
    x: Expr,
    oa: &Expr,
    y: Expr,
    ob: &Expr,
    al: &mut Allocator,
) -> (Expr, Expr) {
    let direct = (x.signature() == oa.signature()) as u32 + (y.signature() == ob.signature()) as u32;
    let swapped = (x.signature() == ob.signature()) as u32 + (y.signature() == oa.signature()) as u32;
    if swapped > direct {
        (
            remap_expr(x, Some(ob), al),
            remap_expr(y, Some(oa), al),
        )
    } else {
        (
            remap_expr(x, Some(oa), al),
            remap_expr(y, Some(ob), al),
        )
    }
}
