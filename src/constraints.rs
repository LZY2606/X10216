//! 约束生成：把 AST 编译成带来源范围、稳定键、scope 与泛化环境指纹的扁平操作序列。

use crate::syntax::{AstNode, Expr, NodeId, Program, Span};
use serde::Serialize;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
pub enum Role {
    Fresh,
    Construct,
    Unify,
    Instantiate,
    Generalize,
    PushLam,
    PopLam,
    PushRec,
    PopRec,
    MatchCheck,
}

fn role_tag(r: Role) -> &'static str {
    match r {
        Role::Fresh => "fresh",
        Role::Construct => "con",
        Role::Unify => "unify",
        Role::Instantiate => "inst",
        Role::Generalize => "gen",
        Role::PushLam => "pushlam",
        Role::PopLam => "poplam",
        Role::PushRec => "pushrec",
        Role::PopRec => "poprec",
        Role::MatchCheck => "mcheck",
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
pub enum GroundKind {
    Int,
    Bool,
    Unit,
}

#[derive(Clone, Debug, Serialize)]
#[allow(clippy::large_enum_variant)]
pub enum OpDetail {
    Fresh { var_key: String, note: String },
    Ground { var_key: String, kind: GroundKind },
    MkArrow { var_key: String, param: String, ret: String },
    MkRecord { var_key: String, fields: Vec<(String, String)>, tail: Option<String> },
    MkVariant { var_key: String, fields: Vec<(String, String)>, tail: Option<String> },
    FreshRow { var_key: String },
    Unify { lkey: String, rkey: String, note: String },
    Instantiate { binder: String, var_key: String },
    Generalize { binder: String, var_key: String, is_value: bool },
    PushLam { var_key: String },
    PopLam { var_key: String },
    PushRec { var_key: String },
    PopRec { var_key: String },
    MatchCheck { scrut_key: String, arms: Vec<String> },
}

#[derive(Clone, Debug, Serialize)]
pub struct Op {
    pub key: String,
    pub seq: usize,
    pub role: Role,
    pub span: Span,
    pub node: String,
    pub scope: String,
    pub env_sig: String,
    pub detail: OpDetail,
}

pub struct Generated {
    pub ops: Vec<Op>,
    pub node_type: Vec<(String, String)>,
}

#[derive(Clone)]
struct EnvEntry {
    name: String,
    binder: String,
    var_key: String,
    poly: bool,
}

struct Cg<'a> {
    prog: &'a Program,
    ops: Vec<Op>,
    node_type: Vec<(String, String)>,
    env: Vec<EnvEntry>,
    scope_parts: Vec<String>,
    counter: usize,
}

impl<'a> Cg<'a> {
    fn node(&self, id: &NodeId) -> &'a AstNode {
        &self.prog.nodes[id.idx]
    }

    fn env_sig(&self) -> String {
        let mut parts: Vec<String> = self
            .env
            .iter()
            .map(|e| format!("{}@{}:{}#{}", e.name, e.binder, e.var_key, if e.poly { 'p' } else { 'm' }))
            .collect();
        parts.sort();
        parts.join("|")
    }

    fn scope(&self) -> String {
        self.scope_parts.join(">")
    }

    fn emit(&mut self, node_key: &str, span: Span, role: Role, detail: OpDetail) {
        let suffix = self.counter;
        self.counter += 1;
        let key = format!("{}:{}:{}", node_key, role_tag(role), suffix);
        self.ops.push(Op {
            key,
            seq: self.ops.len(),
            role,
            span,
            node: node_key.to_string(),
            scope: self.scope(),
            env_sig: self.env_sig(),
            detail,
        });
    }

    fn fresh(&mut self, id: &NodeId, note: &str) -> String {
        let span = self.node(id).span;
        let var_key = format!("{}::{}", id.key, note);
        self.emit(&id.key, span, Role::Fresh, OpDetail::Fresh {
            var_key: var_key.clone(),
            note: note.to_string(),
        });
        var_key
    }

    fn new_row_var(&mut self, id: &NodeId, note: &str) -> String {
        let span = self.node(id).span;
        let var_key = format!("{}::{}", id.key, note);
        self.emit(&id.key, span, Role::Construct, OpDetail::FreshRow {
            var_key: var_key.clone(),
        });
        var_key
    }

    fn unify(&mut self, id: &NodeId, lkey: &str, rkey: &str, note: &str) {
        let span = self.node(id).span;
        self.emit(&id.key, span, Role::Unify, OpDetail::Unify {
            lkey: lkey.to_string(),
            rkey: rkey.to_string(),
            note: note.to_string(),
        });
    }

    fn ground(&mut self, id: &NodeId, note: &str, kind: GroundKind) -> String {
        let span = self.node(id).span;
        let var_key = format!("{}::{}", id.key, note);
        self.emit(&id.key, span, Role::Construct, OpDetail::Ground {
            var_key: var_key.clone(),
            kind,
        });
        var_key
    }

    fn arrow(&mut self, id: &NodeId, note: &str, param: &str, ret: &str) -> String {
        let span = self.node(id).span;
        let var_key = format!("{}::{}", id.key, note);
        self.emit(&id.key, span, Role::Construct, OpDetail::MkArrow {
            var_key: var_key.clone(),
            param: param.to_string(),
            ret: ret.to_string(),
        });
        var_key
    }

    fn record(&mut self, id: &NodeId, fields: Vec<(String, String)>, tail: Option<String>, note: &str) -> String {
        let span = self.node(id).span;
        let var_key = format!("{}::{}", id.key, note);
        self.emit(&id.key, span, Role::Construct, OpDetail::MkRecord {
            var_key: var_key.clone(),
            fields,
            tail,
        });
        var_key
    }

    fn variant(&mut self, id: &NodeId, fields: Vec<(String, String)>, tail: Option<String>, note: &str) -> String {
        let span = self.node(id).span;
        let var_key = format!("{}::{}", id.key, note);
        self.emit(&id.key, span, Role::Construct, OpDetail::MkVariant {
            var_key: var_key.clone(),
            fields,
            tail,
        });
        var_key
    }
}

impl<'a> Cg<'a> {
    fn gen(&mut self, id: &NodeId) -> String {
        let t = self.gen_inner(id);
        self.node_type.push((id.key.clone(), t.clone()));
        t
    }

    fn lookup(&self, name: &str) -> Option<&EnvEntry> {
        self.env.iter().rev().find(|e| e.name == name)
    }

    fn gen_inner(&mut self, id: &NodeId) -> String {
        let node = self.node(id).clone();
        match &node.expr {
            Expr::Int(_) => {
                let t = self.fresh(id, "int");
                let g = self.ground(id, "gint", GroundKind::Int);
                self.unify(id, &t, &g, "整数字面量 : Int");
                t
            }
            Expr::Bool(_) => {
                let t = self.fresh(id, "bool");
                let g = self.ground(id, "gbool", GroundKind::Bool);
                self.unify(id, &t, &g, "布尔字面量 : Bool");
                t
            }
            Expr::Var(name) => {
                let t = self.fresh(id, "use");
                if let Some(e) = self.lookup(name).cloned() {
                    if e.poly {
                        self.emit(&id.key, node.span, Role::Instantiate, OpDetail::Instantiate {
                            binder: e.binder.clone(),
                            var_key: t.clone(),
                        });
                    } else {
                        self.unify(id, &t, &e.var_key, "单态引用与定义点共享");
                    }
                }
                t
            }
            Expr::Fun { param, body } => {
                let pt = self.fresh(id, "param");
                let rt = self.fresh(id, "ret");
                let ft = self.fresh(id, "fun");
                self.emit(&id.key, node.span, Role::PushLam, OpDetail::PushLam {
                    var_key: pt.clone(),
                });
                self.env.push(EnvEntry {
                    name: param.clone(),
                    binder: id.key.clone(),
                    var_key: pt.clone(),
                    poly: false,
                });
                self.scope_parts.push(id.key.clone());
                let bt = self.gen(body);
                self.scope_parts.pop();
                self.env.pop();
                self.emit(&id.key, node.span, Role::PopLam, OpDetail::PopLam {
                    var_key: pt.clone(),
                });
                self.unify(id, &rt, &bt, "函数体类型即返回类型");
                let arr = self.arrow(id, "arrow", &pt, &rt);
                self.unify(id, &ft, &arr, "函数类型 = 参数 -> 返回");
                ft
            }
            Expr::App { fun, arg } => {
                let ft = self.gen(fun);
                let at = self.gen(arg);
                let rt = self.fresh(id, "appret");
                let arr = self.arrow(id, "apparrow", &at, &rt);
                self.unify(id, &ft, &arr, "被调用位置必须是函数");
                rt
            }
            Expr::Record { fields } => {
                let rt = self.fresh(id, "rec");
                let mut field_keys = Vec::new();
                for (label, fid) in fields {
                    let ft = self.gen(fid);
                    field_keys.push((label.clone(), ft));
                }
                let rowt = self.record(id, field_keys, None, "recrow");
                self.unify(id, &rt, &rowt, "记录字面量构造封闭记录");
                rt
            }
            Expr::Proj { record, label } => {
                let rt = self.gen(record);
                let ft = self.fresh(id, "field");
                let rest = self.new_row_var(id, "projrest");
                let rowt = self.record(
                    id,
                    vec![(label.clone(), ft.clone())],
                    Some(rest),
                    "projrow",
                );
                self.unify(id, &rt, &rowt, &format!("投影 .{} 需要该字段", label));
                ft
            }
            Expr::Tag { tag, payload } => {
                let vt = self.fresh(id, "tag");
                let payload_key = match payload {
                    Some(p) => self.gen(p),
                    None => self.ground(id, "gunit", GroundKind::Unit),
                };
                let rowt = self.variant(
                    id,
                    vec![(tag.clone(), payload_key)],
                    None,
                    "tagrow",
                );
                self.unify(id, &vt, &rowt, &format!("变体构造子 {}", tag));
                vt
            }
            Expr::Extend { label, value, base, other } => {
                let val_t = self.gen(value);
                let base_t = self.gen(base);
                let other_t = self.gen(other);
                let ext_t = self.fresh(id, "ext");
                // 两侧行一致
                self.unify(id, &base_t, &other_t, "扩展两侧记录行一致");
                let fb = self.fresh(id, "extfbase");
                let fo = self.fresh(id, "extfother");
                let rb = self.new_row_var(id, "extrestb");
                let ro = self.new_row_var(id, "extresto");
                let row_b = self.record(id, vec![(label.clone(), fb.clone())], Some(rb), "extrowb");
                let row_o = self.record(id, vec![(label.clone(), fo.clone())], Some(ro), "extrowo");
                self.unify(id, &base_t, &row_b, "被扩展记录含该字段");
                self.unify(id, &other_t, &row_o, "来源记录含该字段");
                self.unify(id, &fb, &fo, "同名字段类型一致");
                self.unify(id, &fb, &val_t, "新值类型赋给字段");
                let res_rest = self.new_row_var(id, "extresrest");
                let res = self.record(id, vec![(label.clone(), val_t)], Some(res_rest), "extres");
                self.unify(id, &ext_t, &res, "扩展结果是含该字段的记录");
                ext_t
            }
            Expr::Let { rec, name, bind, body } => self.gen_let(id, *rec, name, *bind, *body),
            Expr::Match { scrut, arms } => {
                let scrut_t = self.gen(scrut);
                let result = self.fresh(id, "matchres");
                let mut tags = Vec::new();
                for (i, arm) in arms.iter().enumerate() {
                    tags.push(arm.tag.clone());
                    let payload = self.fresh(id, &format!("armpayload{}", i));
                    let rest = self.new_row_var(id, &format!("armrest{}", i));
                    let rowt = self.variant(
                        id,
                        vec![(arm.tag.clone(), payload.clone())],
                        Some(rest),
                        &format!("armrow{}", i),
                    );
                    self.unify(id, &scrut_t, &rowt, &format!("分支标签 {}", arm.tag));
                    if arm.binder.is_none() {
                        let u = self.ground(id, &format!("armunit{}", i), GroundKind::Unit);
                        self.unify(id, &payload, &u, "无载荷标签的载荷为 ()");
                    } else {
                        self.env.push(EnvEntry {
                            name: arm.binder.clone().unwrap(),
                            binder: arm.body.key.clone(),
                            var_key: payload.clone(),
                            poly: false,
                        });
                    }
                    self.scope_parts.push(format!("{}.arm{}", id.key, i));
                    let body_t = self.gen(arm.body);
                    self.scope_parts.pop();
                    if arm.binder.is_some() {
                        self.env.pop();
                    }
                    self.unify(id, &result, &body_t, "match 各分支结果一致");
                }
                self.emit(&id.key, node.span, Role::MatchCheck, OpDetail::MatchCheck {
                    scrut_key: scrut_t,
                    arms: tags,
                });
                result
            }
        }
    }

    fn gen_let(
        &mut self,
        id: &NodeId,
        rec: bool,
        name: &str,
        bind: NodeId,
        body: NodeId,
    ) -> String {
        let node = self.node(id).clone();
        let def = self.fresh(id, "def");

        if rec {
            // 递归：自引用在绑定内是单态刚性变量；value restriction 下递归绑定不泛化。
            self.emit(&id.key, node.span, Role::PushRec, OpDetail::PushRec {
                var_key: def.clone(),
            });
            self.env.push(EnvEntry {
                name: name.to_string(),
                binder: id.key.clone(),
                var_key: def.clone(),
                poly: false,
            });
            self.scope_parts.push(id.key.clone());
            let bt = self.gen(bind);
            self.scope_parts.pop();
            self.env.pop();
            self.emit(&id.key, node.span, Role::PopRec, OpDetail::PopRec {
                var_key: def.clone(),
            });
            self.unify(id, &def, &bt, "递归绑定定义类型");

            self.scope_parts.push(format!("{}.in", id.key));
            self.env.push(EnvEntry {
                name: name.to_string(),
                binder: id.key.clone(),
                var_key: def.clone(),
                poly: false,
            });
            let result = self.gen(body);
            self.env.pop();
            self.scope_parts.pop();
            result
        } else {
            self.scope_parts.push(format!("{}.bind", id.key));
            let bt = self.gen(bind);
            self.scope_parts.pop();
            self.unify(id, &def, &bt, "let 定义类型");
            let is_value = is_value_expr(self.prog, &bind);
            self.emit(&id.key, node.span, Role::Generalize, OpDetail::Generalize {
                binder: id.key.clone(),
                var_key: def.clone(),
                is_value,
            });
            self.scope_parts.push(format!("{}.in", id.key));
            self.env.push(EnvEntry {
                name: name.to_string(),
                binder: id.key.clone(),
                var_key: def.clone(),
                poly: is_value,
            });
            let result = self.gen(body);
            self.env.pop();
            self.scope_parts.pop();
            result
        }
    }
}

/// value restriction：只有“语法值”允许泛化。
fn is_value_expr(prog: &Program, id: &NodeId) -> bool {
    match &prog.nodes[id.idx].expr {
        Expr::Int(_) | Expr::Bool(_) | Expr::Tag { .. } => true,
        Expr::Fun { .. } => true,
        Expr::Var(_) => true,
        Expr::Record { fields } => fields.iter().all(|(_, fid)| is_value_expr(prog, fid)),
        _ => false,
    }
}

pub fn generate(prog: &Program) -> Generated {
    let mut cg = Cg {
        prog,
        ops: Vec::new(),
        node_type: Vec::new(),
        env: Vec::new(),
        scope_parts: Vec::new(),
        counter: 0,
    };
    let root_t = cg.gen(&prog.root);
    let _ = root_t;
    Generated { ops: cg.ops, node_type: cg.node_type }
}
