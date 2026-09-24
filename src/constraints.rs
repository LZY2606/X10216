//! 约束与约束生成（带来源范围、稳定约束 id）。

use crate::ast::*;
use crate::types::*;
use std::collections::BTreeMap;

pub type Cid = String;

#[derive(Clone, Debug, PartialEq)]
pub struct Constraint {
    pub id: Cid,
    pub kind: CKind,
    pub node_id: NodeId,
    pub span: Span,
    pub note: String,
    /// 生成顺序（全局稳定）
    pub order: u32,
}

/// 方案模板：泛化时以稳定编号保存类型骨架。
/// `SchemeVar(k)` 是被量词约束的变量；`Ambient(gid)` 是未被泛化的环境变量
/// （value restriction 时出现），实例化必须回指原变量，不得复制。
#[derive(Clone, Debug, PartialEq)]
pub enum SchemeTy {
    SchemeVar(u32),
    Ambient(u32),
    Int,
    Bool,
    Str,
    Fun(Box<SchemeTy>, Box<SchemeTy>),
    Record(Vec<(String, SchemeTy)>, Option<Box<SchemeTy>>),
    Variant(Vec<(String, Option<SchemeTy>)>, Option<Box<SchemeTy>>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum CKind {
    Unify { a: u32, b: u32, label: String },
    Instantiate { name: String, target: u32, bound_at: NodeId },
    Primitive { name: String, target: u32 },
    Generalize {
        name: String,
        bound_at: NodeId,
        value_ty: u32,
        syntactic_value: bool,
        scope_level: u32,
    },
    Unresolved { name: String },
}

#[derive(Clone, Debug)]
enum EnvEntry {
    /// 指向泛化点（使用时需要实例化）
    Scheme { bound_at: NodeId },
    /// 递归自引用：直接共享单态变量
    Direct(u32),
    Primitive(&'static str),
}

pub struct CGen {
    pub store: TypeStore,
    pub cons: Vec<Constraint>,
    pub node_tys: BTreeMap<NodeId, u32>,
    pub decl_tys: BTreeMap<String, u32>,
    node_intern: BTreeMap<String, u32>,
    order: u32,
    level: u32,
}

fn is_syntactic_value(e: &Node) -> bool {
    match &e.kind {
        Expr_::Int(_) | Expr_::Bool(_) | Expr_::Str(_) | Expr_::Var(_) => true,
        Expr_::Fun { .. } => true,
        Expr_::Variant { payload, .. } => payload.as_deref().map(is_syntactic_value).unwrap_or(true),
        Expr_::Record { fields } => fields.iter().all(|(_, e)| is_syntactic_value(e)),
        Expr_::Let { value, body, .. } => is_syntactic_value(value) && is_syntactic_value(body),
        _ => false,
    }
}

impl CGen {
    pub fn new() -> Self {
        CGen {
            store: TypeStore::new(),
            cons: Vec::new(),
            node_tys: BTreeMap::new(),
            decl_tys: BTreeMap::new(),
            node_intern: BTreeMap::new(),
            order: 0,
            level: 1,
        }
    }

    fn node_origin(&mut self, node_id: &str) -> VarOrigin {
        if let Some(id) = self.node_intern.get(node_id) {
            VarOrigin::Node { node_id_id: *id }
        } else {
            let id = self.node_intern.len() as u32;
            self.node_intern.insert(node_id.to_string(), id);
            VarOrigin::Node { node_id_id: id }
        }
    }

    fn ty_for(&mut self, node_id: &str) -> u32 {
        if let Some(t) = self.node_tys.get(node_id) {
            return *t;
        }
        let origin = self.node_origin(node_id);
        let t = self.store.fresh(origin);
        self.node_tys.insert(node_id.to_string(), t);
        t
    }

    fn emit(&mut self, c: CKind, node_id: &str, span: Span, note: impl Into<String>) {
        let role = match &c {
            CKind::Unify { label, .. } => format!("u:{}", label),
            CKind::Instantiate { name, .. } => format!("inst:{}", name),
            CKind::Primitive { name, .. } => format!("prim:{}", name),
            CKind::Generalize { name, .. } => format!("gen:{}", name),
            CKind::Unresolved { name } => format!("unresolved:{}", name),
        };
        let id = format!("c:{}:{}", node_id.trim_start_matches('r'), role);
        let order = self.order;
        self.order += 1;
        self.cons.push(Constraint {
            id,
            kind: c,
            node_id: node_id.to_string(),
            span,
            note: note.into(),
            order,
        });
    }

    fn prim_scheme(&self, name: &str) -> Option<&'static str> {
        match name {
            "int_add" | "int_sub" | "int_mul" => Some("int->int->int"),
            "int_eq" | "int_lt" => Some("int->int->bool"),
            "bool_and" | "bool_or" => Some("bool->bool->bool"),
            "bool_not" => Some("bool->bool"),
            "str_eq" => Some("str->str->bool"),
            "str_cat" => Some("str->str->str"),
            _ => None,
        }
    }

    pub fn gen_program(&mut self, prog: &Program) {
        let mut env: BTreeMap<String, EnvEntry> = BTreeMap::new();
        for prim in [
            "int_add", "int_sub", "int_mul", "int_eq", "int_lt",
            "bool_and", "bool_or", "bool_not", "str_eq", "str_cat",
        ] {
            env.insert(prim.to_string(), EnvEntry::Primitive(prim));
        }
        for decl in &prog.decls {
            self.level += 1;
            let vt = self.gen_decl(decl, &mut env);
            self.decl_tys.insert(decl.id.clone(), vt);
        }
        if let Some(r) = &prog.result {
            let t = self.gen_node(r, &env);
            self.decl_tys.insert("result".to_string(), t);
        }
    }

    fn gen_decl(&mut self, decl: &Decl, env: &mut BTreeMap<String, EnvEntry>) -> u32 {
        let bound_at = format!("{}/e", decl.id);
        if decl.rec {
            let t = self.ty_for(&decl.value.id);
            env.insert(decl.name.clone(), EnvEntry::Direct(t));
            self.gen_node(&decl.value, env);
            self.emit(
                CKind::Generalize {
                    name: decl.name.clone(),
                    bound_at: bound_at.clone(),
                    value_ty: t,
                    syntactic_value: is_syntactic_value(&decl.value),
                    scope_level: self.level,
                },
                &decl.value.id,
                decl.span,
                format!("泛化顶层（递归）绑定 `{}`", decl.name),
            );
            env.insert(decl.name.clone(), EnvEntry::Scheme { bound_at });
            t
        } else {
            let t = self.gen_node(&decl.value, env);
            self.emit(
                CKind::Generalize {
                    name: decl.name.clone(),
                    bound_at: bound_at.clone(),
                    value_ty: t,
                    syntactic_value: is_syntactic_value(&decl.value),
                    scope_level: self.level,
                },
                &decl.value.id,
                decl.span,
                format!("泛化顶层绑定 `{}`", decl.name),
            );
            env.insert(decl.name.clone(), EnvEntry::Scheme { bound_at });
            t
        }
    }

    fn gen_node(&mut self, node: &Node, env: &BTreeMap<String, EnvEntry>) -> u32 {
        match &node.kind {
            Expr_::Int(_) => {
                let t = self.ty_for(&node.id);
                self.emit_unify(t, self.int_ty(), node, "整数字面量：Int".to_string(), "lit-int");
                t
            }
            Expr_::Bool(_) => {
                let t = self.ty_for(&node.id);
                self.emit_unify(t, self.bool_ty(), node, "布尔字面量：Bool".to_string(), "lit-bool");
                t
            }
            Expr_::Str(_) => {
                let t = self.ty_for(&node.id);
                self.emit_unify(t, self.str_ty(), node, "字符串字面量：Str".to_string(), "lit-str");
                t
            }
            Expr_::Var(name) => {
                let t = self.ty_for(&node.id);
                match env.get(name) {
                    Some(EnvEntry::Primitive(_)) => {
                        self.emit(
                            CKind::Primitive { name: name.clone(), target: t },
                            &node.id,
                            node.span,
                            format!("内置函数 `{}` 的类型方案", name),
                        );
                    }
                    Some(EnvEntry::Scheme { bound_at }) => {
                        self.emit(
                            CKind::Instantiate {
                                name: name.clone(),
                                target: t,
                                bound_at: bound_at.clone(),
                            },
                            &node.id,
                            node.span,
                            format!("使用多态变量 `{}`：实例化其方案", name),
                        );
                    }
                    Some(EnvEntry::Direct(ty)) => {
                        let b = *ty;
                        self.emit_unify(t, b, node, format!("递归引用 `{}` 共享单态类型", name), "rec-ref");
                    }
                    None => {
                        self.emit(
                            CKind::Unresolved { name: name.clone() },
                            &node.id,
                            node.span,
                            format!("名字 `{}` 未在当前作用域绑定", name),
                        );
                    }
                }
                t
            }
            Expr_::Fun { param, body } => {
                let t = self.ty_for(&node.id);
                let pt = self.ty_for(&format!("{}#p:{}", node.id, param));
                let rt = self.gen_node(body, &Self::extend_env(env, param.clone(), EnvEntry::Direct(pt)));
                let fun_ty = self.store.make(Type_::TFun(pt, rt), self.node_origin(&node.id));
                self.emit_unify(t, fun_ty, node, format!("函数 `{}` -> 函数类型", param), "fun");
                t
            }
            Expr_::App { fun, arg } => {
                let t = self.ty_for(&node.id);
                let ft = self.gen_node(fun, env);
                let at = self.gen_node(arg, env);
                let rt = self.ty_for(&format!("{}#ret", node.id));
                let fty = self.store.make(Type_::TFun(at, rt), self.node_origin(&node.id));
                self.emit_unify(ft, fty, node, "应用：函数必须是 `参数 -> 结果`".to_string(), "app-fun");
                self.emit_unify(t, rt, node, "应用结果类型".to_string(), "app-ret");
                t
            }
            Expr_::If { cond, then_b, else_b } => {
                let t = self.ty_for(&node.id);
                let ct = self.gen_node(cond, env);
                let tt = self.gen_node(then_b, env);
                let et = self.gen_node(else_b, env);
                self.emit_unify(ct, self.bool_ty(), node, "if 条件必须为 Bool".to_string(), "if-cond");
                self.emit_unify(t, tt, then_b, "then 分支即整体类型".to_string(), "if-then");
                self.emit_unify(tt, et, else_b, "两个分支类型必须一致".to_string(), "if-branches");
                t
            }
            Expr_::Let { rec, name, value, body, .. } => {
                let t = self.ty_for(&node.id);
                self.level += 1;
                let mut local = env.clone();
                let bound_at = value.id.clone();
                let vt = if *rec {
                    let vv = self.ty_for(&value.id);
                    local.insert(name.clone(), EnvEntry::Direct(vv));
                    let got = self.gen_node(value, &local);
                    self.emit_unify(vv, got, value, format!("递归绑定 `{}` 类型一致", name), "rec-self");
                    vv
                } else {
                    self.gen_node(value, env)
                };
                self.emit(
                    CKind::Generalize {
                        name: name.clone(),
                        bound_at: bound_at.clone(),
                        value_ty: vt,
                        syntactic_value: is_syntactic_value(value),
                        scope_level: self.level,
                    },
                    &value.id,
                    Span::merge(node.span, value.span),
                    format!("let 绑定 `{}`：在进入 body 前泛化", name),
                );
                local.insert(name.clone(), EnvEntry::Scheme { bound_at });
                let bt = self.gen_node(body, &local);
                self.emit_unify(t, bt, body, "let 表达式类型即 body 类型".to_string(), "let-body");
                self.level -= 1;
                t
            }
            Expr_::Record { fields } => {
                let t = self.ty_for(&node.id);
                let mut mapped: Vec<(String, u32)> = Vec::new();
                for (label, e) in fields {
                    let ft = self.gen_node(e, env);
                    mapped.push((label.clone(), ft));
                }
                let rec_ty = self.store.make(
                    Type_::Record(TypeStore::record_fields_sorted(mapped), None),
                    self.node_origin(&node.id),
                );
                self.emit_unify(t, rec_ty, node, "记录构造：闭合行".to_string(), "record");
                t
            }
            Expr_::Proj { record, label } => {
                let t = self.ty_for(&node.id);
                let rt = self.gen_node(record, env);
                let field_ty = self.ty_for(&format!("{}#field:{}", node.id, label));
                // 开放行：含 label 字段，其余任意
                let (tailv, _) = self.store.fresh_row_tail();
                let row_ty = self.store.make(
                    Type_::Record(vec![(label.clone(), field_ty)], tailv),
                    self.node_origin(&node.id),
                );
                self.emit_unify(rt, row_ty, node, format!("投影 `.{}`：记录必须含该字段（开放行）", label), "proj");
                self.emit_unify(t, field_ty, node, format!("`.{}` 的结果类型", label), "proj-ret");
                t
            }
            Expr_::Extend { label, value, rest } => {
                let t = self.ty_for(&node.id);
                let vt = self.gen_node(value, env);
                let rt = self.gen_node(rest, env);
                let (tailv, _) = self.store.fresh_row_tail();
                let extended = self.store.make(
                    Type_::Record(vec![(label.clone(), vt)], tailv),
                    self.node_origin(&node.id),
                );
                self.emit_unify(rt, extended, node, format!("行扩展：补入字段 `{}`", label), "extend-rest");
                let (tailv2, _) = self.store.fresh_row_tail();
                let full = self.store.make(
                    Type_::Record(vec![(label.clone(), vt)], tailv2),
                    self.node_origin(&node.id),
                );
                // rest 行可能已经含同名字段：上面的统一会触发“多余字段”
                self.emit_unify(t, full, node, "扩展结果记录类型".to_string(), "extend-out");
                t
            }
            Expr_::Variant { label, payload } => {
                let t = self.ty_for(&node.id);
                let payload_ty = if let Some(p) = payload {
                    Some(self.gen_node(p, env))
                } else {
                    None
                };
                let (tailv, _) = self.store.fresh_row_tail();
                let v_ty = self.store.make(
                    Type_::Variant(vec![(label.clone(), payload_ty)], tailv),
                    self.node_origin(&node.id),
                );
                self.emit_unify(t, v_ty, node, format!("变体注入 `{}`（开放行）", label), "variant");
                t
            }
            Expr_::Match { scrut, arms } => {
                let t = self.ty_for(&node.id);
                let st = self.gen_node(scrut, env);
                let mut arm_result_tys = Vec::new();
                for (idx, (pat, body)) in arms.iter().enumerate() {
                    let mut local = env.clone();
                    if let Pat::PVar(v) = pat {
                        local.insert(v.clone(), EnvEntry::Direct(st));
                    }
                    if let Pat::PTag(label, payload_name) = pat {
                        if let Some(vn) = payload_name {
                            let pt = self.ty_for(&format!("{}#arm{}#p:{}", node.id, idx, label));
                            local.insert(vn.clone(), EnvEntry::Direct(pt));
                            let (tailv, _) = self.store.fresh_row_tail();
                            let tag_row = self.store.make(
                                Type_::Variant(vec![(label.clone(), Some(pt))], tailv),
                                self.node_origin(&node.id),
                            );
                            self.emit_unify(st, tag_row, node, format!("match 分支要求标签 `{}` 存在，且载荷类型匹配", label), "match-tag");
                        } else {
                            let (tailv, _) = self.store.fresh_row_tail();
                            let tag_row = self.store.make(
                                Type_::Variant(vec![(label.clone(), None)], tailv),
                                self.node_origin(&node.id),
                            );
                            self.emit_unify(st, tag_row, node, format!("match 分支要求标签 `{}` 存在（无载荷）", label), "match-tag");
                        }
                    }
                    let bt = self.gen_node(body, &local);
                    arm_result_tys.push(bt);
                }
                // 分支结果一致
                let first = arm_result_tys[0];
                for bt in &arm_result_tys[1..] {
                    self.emit_unify(first, *bt, scrut, "match 各分支结果类型必须一致".to_string(), "match-arms");
                }
                self.emit_unify(t, first, scrut, "match 结果类型".to_string(), "match-ret");
                t
            }
        }
    }

    fn extend_env(
        env: &BTreeMap<String, EnvEntry>,
        name: String,
        entry: EnvEntry,
    ) -> BTreeMap<String, EnvEntry> {
        let mut e = env.clone();
        e.insert(name, entry);
        e
    }

    fn emit_unify(&mut self, a: u32, b: u32, node: &Node, note: String, label: &str) {
        self.emit(CKind::Unify { a, b, label: label.to_string() }, &node.id, node.span, note);
    }

    fn int_ty(&mut self) -> u32 {
        self.store.make(Type_::TInt, VarOrigin::Node { node_id_id: u32::MAX })
    }
    fn bool_ty(&mut self) -> u32 {
        self.store.make(Type_::TBool, VarOrigin::Node { node_id_id: u32::MAX })
    }
    fn str_ty(&mut self) -> u32 {
        self.store.make(Type_::TStr, VarOrigin::Node { node_id_id: u32::MAX })
    }
}
