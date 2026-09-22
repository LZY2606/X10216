// Constraint generation for the HM language with rows and variants.

use crate::constraints::*;
use crate::syntax::*;
use crate::term::*;
use crate::types::FlexKind;
use std::collections::HashMap;

#[derive(Clone, Debug)]
enum Binding {
    Monomorphic {
        ty: NodeIdT,
        let_path: String,
        rec: bool,
    },
    Scheme {
        quantified: Vec<NodeIdT>,
        body: NodeIdT,
        origin_let: CId,
    },
    Ambient {
        // Built concrete term describing the ambient scheme; instantiated on use.
        root: NodeIdT,
    },
}

struct GEnv {
    map: HashMap<String, Vec<Binding>>,
}

impl GEnv {
    fn new() -> Self {
        GEnv { map: HashMap::new() }
    }
    fn bind(&mut self, name: String, b: Binding) {
        self.map.entry(name).or_default().push(b);
    }
    fn lookup(&self, name: &str) -> Option<&Binding> {
        self.map.get(name).and_then(|v| v.last())
    }
    fn pop(&mut self, name: &str) {
        if let Some(v) = self.map.get_mut(name) {
            v.pop();
        }
    }
}

pub struct Generated {
    pub graph: ConstraintGraph,
    pub bases: HashMap<NodeIdT, BaseS>,
    pub flexes: HashMap<NodeIdT, FlexDesc>,
    pub ambient_inst_roots: HashMap<CId, NodeIdT>,
    pub scheme_insts: HashMap<CId, (Vec<NodeIdT>, NodeIdT)>,
}

pub struct Generator<'a> {
    src: &'a str,
    nodes: Vec<ConstraintNode>,
    type_nodes: Vec<TypeNodeInfo>,
    ast_nodes: Vec<AstNodeInfo>,
    expr_type: HashMap<NodeIdT, NodeIdT>,
    ast_paths: HashMap<NodeIdT, String>,
    env: GEnv,
    next_c: u32,
    next_t: u32,
    next_n: u32,
    fp: GenFingerprint,
    scope_stack: Vec<String>,
    key_ordinals: HashMap<String, u32>,
    /// Concrete descriptors for generator-created type nodes.
    pub bases: HashMap<NodeIdT, BaseS>,
    pub flexes: HashMap<NodeIdT, FlexDesc>,
    /// Inst node id -> ambient concrete root (for solver copying).
    pub ambient_inst_roots: HashMap<CId, NodeIdT>,
    /// Inst node id -> (quantified vars, scheme body root).
    pub scheme_insts: HashMap<CId, (Vec<NodeIdT>, NodeIdT)>,
}

impl<'a> Generator<'a> {
    pub fn new(src: &'a str) -> Self {
        Generator {
            src,
            nodes: Vec::new(),
            type_nodes: Vec::new(),
            ast_nodes: Vec::new(),
            expr_type: HashMap::new(),
            ast_paths: HashMap::new(),
            env: GEnv::new(),
            next_c: 1,
            next_t: 1,
            next_n: 1,
            fp: GenFingerprint::default(),
            scope_stack: Vec::new(),
            key_ordinals: HashMap::new(),
            bases: HashMap::new(),
            flexes: HashMap::new(),
            ambient_inst_roots: HashMap::new(),
            scheme_insts: HashMap::new(),
        }
    }

    fn cid(&mut self) -> CId {
        let id = CId(self.next_c);
        self.next_c += 1;
        id
    }
    fn tid(&mut self) -> NodeIdT {
        let id = NodeIdT(self.next_t);
        self.next_t += 1;
        id
    }
    fn nid(&mut self) -> NodeIdT {
        let id = NodeIdT(self.next_n);
        self.next_n += 1;
        id
    }

    fn add_c(&mut self, mut node: ConstraintNode, parent: CId) -> CId {
        let id = node.id;
        node.parent = Some(parent);
        self.nodes.push(node);
        if let Some(p) = self.nodes.iter_mut().find(|n| n.id == parent) {
            p.children.push(id);
        }
        id
    }

    fn block(&mut self, parent: CId, span: Span) -> CId {
        let id = self.cid();
        let key = self.key(span, "", "block", 0);
        self.nodes.push(ConstraintNode {
            id,
            parent: Some(parent),
            children: Vec::new(),
            kind: CKind::Block,
            span,
            origin: None,
            key,
            disabled: false,
        });
        if let Some(p) = self.nodes.iter_mut().find(|n| n.id == parent) {
            p.children.push(id);
        }
        id
    }

    fn key(&mut self, span: Span, path: &str, tag: &str, ordinal: u32) -> ConstraintKey {
        let scope = self.scope_stack.join(">");
        let gen_env = self.fp.string();
        let proto = format!("{}|{}|{}|{}|{}", path, tag, ordinal, scope, gen_env);
        let n = self.key_ordinals.entry(proto.clone()).or_insert(0);
        // Keys already carry ordinal explicitly; counter guarantees uniqueness
        // across repeated structural shapes.
        let unique = format!("{}#{}", proto, *n);
        *n += 1;
        let _ = span;
        let _ = unique;
        ConstraintKey {
            path: path.into(),
            tag: tag.into(),
            ordinal,
            scope,
            gen_env,
        }
    }

    fn fresh_flex(&mut self, rank: u32, hint: impl Into<String>, expr: Option<NodeIdT>, span: Option<Span>) -> NodeIdT {
        let id = self.tid();
        let hint = hint.into();
        self.flexes.insert(id, FlexDesc { kind: FlexKind::Type, rank, hint: hint.clone() });
        self.type_nodes.push(TypeNodeInfo {
            id,
            expr,
            span,
            hint,
            flex_row: false,
            rigid: None,
            concrete: false,
        });
        id
    }

    fn fresh_row(&mut self, rank: u32, expr: Option<NodeIdT>, span: Option<Span>) -> NodeIdT {
        let id = self.tid();
        self.flexes.insert(id, FlexDesc { kind: FlexKind::Row, rank, hint: String::new() });
        self.type_nodes.push(TypeNodeInfo {
            id,
            expr,
            span,
            hint: String::new(),
            flex_row: true,
            rigid: None,
            concrete: false,
        });
        id
    }

    fn fresh_rigid(&mut self, name: &str, expr: Option<NodeIdT>, span: Span) -> NodeIdT {
        let id = self.tid();
        self.type_nodes.push(TypeNodeInfo {
            id,
            expr,
            span: Some(span),
            hint: name.into(),
            flex_row: false,
            rigid: Some(name.into()),
            concrete: false,
        });
        id
    }

    fn concrete(&mut self, base: BaseS, expr: Option<NodeIdT>, span: Option<Span>) -> NodeIdT {
        let id = self.tid();
        self.bases.insert(id, base);
        self.type_nodes.push(TypeNodeInfo {
            id,
            expr,
            span,
            hint: String::new(),
            flex_row: false,
            rigid: None,
            concrete: true,
        });
        id
    }

    fn add_eq(
        &mut self,
        parent: CId,
        path: &str,
        ordinal: u32,
        l: NodeIdT,
        r: NodeIdT,
        label: impl Into<String>,
        span: Span,
        origin: Option<NodeIdT>,
    ) -> CId {
        let id = self.cid();
        let node = ConstraintNode {
            id,
            parent: None,
            children: Vec::new(),
            kind: CKind::Eq(EqSpec { left: l, right: r, label: label.into() }),
            span,
            origin,
            key: self.key(span, path, "eq", ordinal),
            disabled: false,
        };
        self.add_c(node, parent)
    }
}

impl<'a> Generator<'a> {
    pub fn generate(mut self, expr: &Expr) -> Generated {
        let root = self.cid();
        self.nodes.push(ConstraintNode {
            id: root,
            parent: None,
            children: Vec::new(),
            kind: CKind::Block,
            span: expr.span(),
            origin: None,
            key: ConstraintKey {
                path: "$root".into(),
                tag: "block".into(),
                ordinal: 0,
                scope: String::new(),
                gen_env: String::new(),
            },
            disabled: false,
        });
        self.install_builtins(root);
        let rank0 = 1;
        let mut path = vec![PathSeg::Root];
        let top_ty = self.gen_expr(expr, root, &mut path, rank0);
        // Final program type equation is represented by the type nodes
        // attached to the root expression.
        let _ = top_ty;
        let graph = ConstraintGraph {
            nodes: self.nodes,
            type_nodes: self.type_nodes,
            ast_nodes: self.ast_nodes,
            root,
            expr_type: self.expr_type,
            ast_paths: self.ast_paths,
        };
        Generated {
            graph,
            bases: self.bases,
            flexes: self.flexes,
            ambient_inst_roots: self.ambient_inst_roots,
            scheme_insts: self.scheme_insts,
        }
    }

    fn install_builtins(&mut self, root: CId) {
        let rank = 1;
        let make_arith = |this: &mut Self| {
            // int -> int -> int
            let a = this.fresh_flex(rank, "", None, None);
            let b = this.fresh_flex(rank, "", None, None);
            let c = this.fresh_flex(rank, "", None, None);
            this.add_eq(root, "$builtin", 0, a, b, "builtin int", Span::new(0, 0), None);
            let ret = this.concrete(BaseS::Arrow(b, c), None, None);
            let top = this.concrete(BaseS::Arrow(a, ret), None, None);
            top
        };
        for name in ["add", "sub", "mul", "div"] {
            let ty = make_arith(self);
            self.env.bind(format!("__builtin_{}", name), Binding::Ambient { root: ty });
        }
        // Comparison: int -> int -> bool
        let a = self.fresh_flex(rank, "", None, None);
        let b = self.fresh_flex(rank, "", None, None);
        self.add_eq(root, "$builtin", 1, a, b, "builtin int", Span::new(0, 0), None);
        let bool1 = self.concrete(BaseS::Con("Bool".into()), None, None);
        let ret = self.concrete(BaseS::Arrow(b, bool1), None, None);
        let cmp = self.concrete(BaseS::Arrow(a, ret), None, None);
        for name in ["eq", "lt", "gt", "le", "ge"] {
            self.env.bind(format!("__builtin_{}", name), Binding::Ambient { root: cmp });
        }
    }

    fn ast_enter(&mut self, expr: &Expr, path: &[PathSeg]) -> (NodeIdT, String) {
        let id = self.nid();
        let path_s = path_string(path);
        let (kind, label) = describe_expr(expr, self.src);
        let mut children = Vec::new();
        visit_collect(expr, &mut children);
        self.ast_nodes.push(AstNodeInfo {
            id,
            path: path_s.clone(),
            span: expr.span(),
            kind,
            label,
            children: Vec::new(),
        });
        self.ast_paths.insert(id, path_s.clone());
        let _ = children;
        (id, path_s)

    }

    fn gen_expr(&mut self, expr: &Expr, parent: CId, path: &mut Vec<PathSeg>, rank: u32) -> NodeIdT {
        let (ast, path_s) = self.ast_enter(expr, path);
        let block = self.block(parent, expr.span());
        let ty = self.gen_expr_inner(expr, block, path, &path_s, rank, ast);
        self.expr_type.insert(ast, ty);
        if let Some(info) = self.type_nodes.get_mut((ty.0 - 1) as usize) {
            if info.expr.is_none() {
                info.expr = Some(ast);
                info.span = Some(expr.span());
            }
        }
        // Link AST children list for the panel (only direct child roots).
        if let Some(node) = self.ast_nodes.iter_mut().find(|n| n.id == ast) {
            node.children = direct_child_ids(expr, &self.ast_paths, path);
        }
        ty
    }
}

impl<'a> Generator<'a> {
    fn gen_expr_inner(
        &mut self,
        expr: &Expr,
        block: CId,
        path: &mut Vec<PathSeg>,
        path_s: &str,
        rank: u32,
        ast: NodeIdT,
    ) -> NodeIdT {
        match expr {
            Expr::Lit(lit, span) => {
                let name = match lit {
                    Lit::Int(_) => "Int",
                    Lit::Bool(_) => "Bool",
                    Lit::Str(_) => "String",
                };
                let ty = self.concrete(BaseS::Con(name.into()), Some(ast), Some(*span));
                ty
            }
            Expr::Var(name, span) => {
                let ty = self.fresh_flex(rank, hint_name(name), Some(ast), Some(*span));
                match self.env.lookup(name).cloned() {
                    Some(Binding::Ambient { root }) => {
                        let id = self.cid();
                        let node = ConstraintNode {
                            id, parent: None, children: Vec::new(),
                            kind: CKind::Inst(InstSpec {
                                name: name.clone(),
                                target: ty,
                                origin_let: CId(0),
                                occurrence: ast,
                            }),
                            span: *span, origin: Some(ast),
                            key: self.key(*span, path_s, "inst:ambient", 0),
                            disabled: false,
                        };
                        self.add_c(node, block);
                        // Save the ambient descriptor root on the node side via a
                        // side table keyed by inst id.
                        self.ambient_inst_roots.insert(id, root);
                    }
                    Some(Binding::Scheme { quantified, body, origin_let }) => {
                        let id = self.cid();
                        let node = ConstraintNode {
                            id, parent: None, children: Vec::new(),
                            kind: CKind::Inst(InstSpec {
                                name: name.clone(),
                                target: ty,
                                origin_let,
                                occurrence: ast,
                            }),
                            span: *span, origin: Some(ast),
                            key: self.key(*span, path_s, "inst:let", 0),
                            disabled: false,
                        };
                        self.add_c(node, block);
                        self.scheme_insts.insert(
                            id,
                            (quantified, body),
                        );
                    }
                    Some(Binding::Monomorphic { ty: src_ty, .. }) => {
                        self.add_eq(
                            block, path_s, 0, ty, src_ty,
                            format!("use `{}` (monomorphic)", name),
                            *span, Some(ast),
                        );
                    }
                    None => {
                        // Unbound variable: emit an equality with a dedicated
                        // rigid node so solving produces a clear error core.
                        let rig = self.fresh_rigid(
                            &format!("?unbound:{}", name), Some(ast), *span);
                        self.add_eq(
                            block, path_s, 0, ty, rig,
                            format!("unbound variable `{}`", name),
                            *span, Some(ast),
                        );
                    }
                }
                ty
            }
            Expr::Lam { param, body, span } => {
                let pty = self.fresh_flex(rank, hint_pat(param), Some(ast), Some(param.span()));
                let old_len = self.scope_stack.len();
                self.scope_stack.push(format!("lam@{}", path_s));
                if let Some(n) = param.binder() {
                    self.env.bind(n.to_string(), Binding::Monomorphic {
                        ty: pty,
                        let_path: path_s.into(),
                        rec: false,
                    });
                }
                path.push(PathSeg::LamBody);
                let bty = self.gen_expr(body, block, path, rank);
                path.pop();
                if let Some(n) = param.binder() {
                    self.env.pop(n);
                }
                self.scope_stack.truncate(old_len);
                let ty = self.concrete(BaseS::Arrow(pty, bty), Some(ast), Some(*span));
                ty
            }
            Expr::App { fun, arg, span } => {
                path.push(PathSeg::AppFun);
                let fty = self.gen_expr(fun, block, path, rank);
                path.pop();
                path.push(PathSeg::AppArg);
                let aty = self.gen_expr(arg, block, path, rank);
                path.pop();
                let rty = self.fresh_flex(rank, "ret", Some(ast), Some(*span));
                let fty2 = self.concrete(BaseS::Arrow(aty, rty), Some(ast), Some(*span));
                self.add_eq(
                    block, path_s, 0, fty, fty2,
                    String::from("application: function has type `arg -> result`"),
                    *span, Some(ast),
                );
                rty
            }
            Expr::Ann { expr: e, ty: tyast, span } => {
                let inner = self.gen_expr(e, block, path, rank);
                let ann = self.ann_ty(tyast, rank, ast);
                self.add_eq(
                    block, path_s, 0, inner, ann,
                    String::from("type annotation"), *span, Some(ast),
                );
                inner
            }
            Expr::If { cond, then_e, else_e, span } => {
                path.push(PathSeg::IfCond);
                let cty = self.gen_expr(cond, block, path, rank);
                path.pop();
                let boolt = self.concrete(BaseS::Con("Bool".into()), Some(ast), Some(*span));
                self.add_eq(block, path_s, 0, cty, boolt, "if-condition must be Bool".to_string(), *span, Some(ast));
                path.push(PathSeg::IfThen);
                let t1 = self.gen_expr(then_e, block, path, rank);
                path.pop();
                path.push(PathSeg::IfElse);
                let t2 = self.gen_expr(else_e, block, path, rank);
                path.pop();
                self.add_eq(block, path_s, 1, t1, t2, "if branches must agree".to_string(), *span, Some(ast));
                t1
            }
            Expr::Let { rec, pat, rhs, body, span } => self.gen_let(
                block, path, path_s, rank, ast, *rec, pat, rhs, body, *span,
            ),
            Expr::Record { fields, span } => {
                let mut fs = Vec::new();
                for (name, e, _fspan) in fields.iter() {
                    path.push(PathSeg::RecordField(name.clone()));
                    let t = self.gen_expr(e, block, path, rank);
                    path.pop();
                    fs.push((name.clone(), t));
                }
                self.concrete(
                    BaseS::Row { sort: RowSortS::Record, fields: fs, tail: TailS::Closed },
                    Some(ast), Some(*span),
                )
            }
            Expr::Extend { base, fields, span } => {
                path.push(PathSeg::ExtendBase);
                let bty = self.gen_expr(base, block, path, rank);
                path.pop();
                // base : { new_fields, ..r }; the whole extension closes r.
                let rowvar = self.fresh_row(rank, Some(ast), Some(*span));
                let mut fs = Vec::new();
                for (name, e, _fspan) in fields.iter() {
                    path.push(PathSeg::ExtendField(name.clone()));
                    let t = self.gen_expr(e, block, path, rank);
                    path.pop();
                    fs.push((name.clone(), t));
                }
                let base_row = self.concrete(BaseS::Row {
                    sort: RowSortS::Record,
                    fields: fs.clone(),
                    tail: TailS::Open(rowvar),
                }, Some(ast), Some(*span));
                self.add_eq(
                    block, path_s, 0, base_row, bty,
                    String::from("record extension: base provides the updated fields and shares the open tail"),
                    *span, Some(ast),
                );
                self.concrete(
                    BaseS::Row {
                        sort: RowSortS::Record,
                        fields: fs,
                        tail: TailS::Open(rowvar),
                    },
                    Some(ast), Some(*span),
                )
            }
            Expr::Field { record, name, name_span, span } => {
                path.push(PathSeg::FieldOf);
                let rty = self.gen_expr(record, block, path, rank);
                path.pop();
                let fty = self.fresh_flex(rank, name, Some(ast), Some(*name_span));
                let rest = self.fresh_row(rank, Some(ast), Some(*span));
                let row = self.concrete(BaseS::Row {
                    sort: RowSortS::Record,
                    fields: vec![(name.clone(), fty)],
                    tail: TailS::Open(rest),
                }, Some(ast), Some(*span));
                self.add_eq(block, path_s, 0, row, rty,
                    format!("record access `.{}`", name), *span, Some(ast));
                fty
            }
            Expr::Variant { label, payload, span } => {
                let pty = match payload {
                    Some(p) => {
                        path.push(PathSeg::VariantPayload);
                        let t = self.gen_expr(p, block, path, rank);
                        path.pop();
                        t
                    }
                    None => self.concrete(BaseS::Con("Unit".into()), Some(ast), Some(*span)),
                };
                let rest = self.fresh_row(rank, Some(ast), Some(*span));
                self.concrete(BaseS::Row {
                    sort: RowSortS::Variant,
                    fields: vec![(label.clone(), pty)],
                    tail: TailS::Open(rest),
                }, Some(ast), Some(*span))
            }
            Expr::Match { scrut, cases, span } => {
                path.push(PathSeg::MatchScrut);
                let sty = self.gen_expr(scrut, block, path, rank);
                path.pop();
                let result = self.fresh_flex(rank, "match", Some(ast), Some(*span));
                let rowvar = self.fresh_row(rank, Some(ast), Some(*span));
                let mut fs = Vec::new();
                let old_scope = self.scope_stack.len();
                self.scope_stack.push(format!("match@{}", path_s));
                for (ci, (label, pat, body, cspan)) in cases.iter().enumerate() {
                    let pty = self.fresh_flex(rank, hint_pat(pat), Some(ast), Some(pat.span()));
                    if let Some(n) = pat.binder() {
                        self.env.bind(n.into(), Binding::Monomorphic {
                            ty: pty, let_path: path_s.into(), rec: false,
                        });
                    }
                    path.push(PathSeg::CaseBody(label.clone()));
                    let bty = self.gen_expr(body, block, path, rank);
                    path.pop();
                    if let Some(n) = pat.binder() {
                        self.env.pop(n);
                    }
                    self.add_eq(block, path_s, ci as u32 + 1, bty, result,
                        format!("case `{}` result", label), *cspan, Some(ast));
                    fs.push((label.clone(), pty));
                }
                self.scope_stack.truncate(old_scope);
                let open_row = self.concrete(BaseS::Row {
                    sort: RowSortS::Variant, fields: fs.clone(), tail: TailS::Open(rowvar),
                }, Some(ast), Some(*span));
                self.add_eq(block, path_s, 0, open_row, sty,
                    String::from("match covers a row of the scrutinee"), *span, Some(ast));
                self.empty_row(block, path_s, rowvar, "variant", *span, ast, 1);
                result
            }
        }
    }
}

impl<'a> Generator<'a> {
    fn empty_row(
        &mut self,
        parent: CId,
        path_s: &str,
        row: NodeIdT,
        sort: &'static str,
        span: Span,
        ast: NodeIdT,
        ordinal: u32,
    ) {
        let id = self.cid();
        let node = ConstraintNode {
            id,
            parent: None,
            children: Vec::new(),
            kind: CKind::EmptyRow(EmptyRowSpec { row, sort }),
            span,
            origin: Some(ast),
            key: self.key(span, path_s, &format!("empty_row:{}", sort), ordinal),
            disabled: false,
        };
        self.add_c(node, parent);
    }

    fn ann_ty(&mut self, ty: &TyAst, rank: u32, ast: NodeIdT) -> NodeIdT {
        match ty {
            TyAst::Var(n, sp) => self.fresh_rigid(n, Some(ast), *sp),
            TyAst::Con(n, sp) if n == "Unit" => {
                self.concrete(BaseS::Con("Unit".into()), Some(ast), Some(*sp))
            }
            TyAst::Con(n, sp) => self.concrete(BaseS::Con(n.clone()), Some(ast), Some(*sp)),
            TyAst::Arrow(a, b, sp) => {
                let ta = self.ann_ty(a, rank, ast);
                let tb = self.ann_ty(b, rank, ast);
                self.concrete(BaseS::Arrow(ta, tb), Some(ast), Some(*sp))
            }
            TyAst::Record(fs, open, sp) => {
                let mut fields = Vec::new();
                for (n, t) in fs {
                    fields.push((n.clone(), self.ann_ty(t, rank, ast)));
                }
                let tail = if *open {
                    TailS::Open(self.fresh_row(rank, Some(ast), Some(*sp)))
                } else {
                    TailS::Closed
                };
                self.concrete(BaseS::Row { sort: RowSortS::Record, fields, tail }, Some(ast), Some(*sp))
            }
            TyAst::Variant(fs, open, sp) => {
                let mut fields = Vec::new();
                for (n, t) in fs {
                    let tt = match t {
                        TyAst::Con(u, usp) if u == "Unit" => {
                            self.concrete(BaseS::Con("Unit".into()), Some(ast), Some(*usp))
                        }
                        other => self.ann_ty(other, rank, ast),
                    };
                    fields.push((n.clone(), tt));
                }
                let tail = if *open {
                    TailS::Open(self.fresh_row(rank, Some(ast), Some(*sp)))
                } else {
                    TailS::Closed
                };
                self.concrete(BaseS::Row { sort: RowSortS::Variant, fields, tail }, Some(ast), Some(*sp))
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn gen_let(
        &mut self,
        block: CId,
        path: &mut Vec<PathSeg>,
        path_s: &str,
        rank: u32,
        ast: NodeIdT,
        rec: bool,
        pat: &Pat,
        rhs: &Expr,
        body: &Expr,
        span: Span,
    ) -> NodeIdT {
        // Let constraint: children are [rhs block, gen node, body block].
        let let_id = self.cid();
        let let_node = ConstraintNode {
            id: let_id,
            parent: Some(block),
            children: Vec::new(),
            kind: CKind::Block,
            span,
            origin: Some(ast),
            key: self.key(span, path_s, "let", 0),
            disabled: false,
        };
        self.add_c(let_node, block);

        let rty = self.fresh_flex(rank, hint_pat(pat), Some(ast), Some(pat.span()));
        let value_form = is_value_form(rhs);

        if rec {
            // Monomorphic recursion: install before RHS.
            if let Some(n) = pat.binder() {
                self.env.bind(n.into(), Binding::Monomorphic {
                    ty: rty, let_path: path_s.into(), rec: true,
                });
            }
        }

        // RHS.
        let rhs_block = self.block(let_id, rhs.span());
        path.push(PathSeg::LetRhs);
        let actual = self.gen_expr(rhs, rhs_block, path, rank + 1);
        path.pop();
        self.add_eq(
            rhs_block, path_s, 0, rty, actual,
            format!("let binding `{}`", pat.binder().unwrap_or("_")),
            span, Some(ast),
        );

        // Generalization node.
        let can_generalize = !rec && value_form;
        let gen_id = self.cid();
        let gen_node = ConstraintNode {
            id: gen_id,
            parent: None,
            children: Vec::new(),
            kind: CKind::Gen(GenSpec {
                name: pat.binder().unwrap_or("_").into(),
                value_restricted: !can_generalize,
                rec,
            }),
            span: pat.span(),
            origin: Some(ast),
            key: self.key(span, path_s, "gen", 0),
            disabled: false,
        };
        self.add_c(gen_node, let_id);

        if rec {
            if let Some(n) = pat.binder() {
                self.env.pop(n);
            }
        }

        // Body binding: generalized scheme or monomorphic.
        let old_scope = self.scope_stack.len();
        self.scope_stack.push(format!("let@{}", path_s));
        if let Some(n) = pat.binder() {
            if can_generalize {
                self.env.bind(n.into(), Binding::Scheme {
                    quantified: Vec::new(),
                    body: rty,
                    origin_let: let_id,
                });
                self.fp.push(path_s, n, true, false);
            } else {
                self.env.bind(n.into(), Binding::Monomorphic {
                    ty: rty,
                    let_path: path_s.into(),
                    rec,
                });
                self.fp.push(path_s, n, false, rec);
            }
        }

        let body_block = self.block(let_id, body.span());
        path.push(PathSeg::LetBody);
        let bty = self.gen_expr(body, body_block, path, rank);
        path.pop();

        if let Some(n) = pat.binder() {
            self.env.pop(n);
        }
        self.fp.pop();
        self.scope_stack.truncate(old_scope);
        bty
    }
}

fn is_value_form(e: &Expr) -> bool {
    // Value restriction: generalize only syntactic values.
    matches!(
        e,
        Expr::Lam { .. }
            | Expr::Lit(..)
            | Expr::Record { .. }
            | Expr::Variant { payload: None, .. }
    ) || matches!(
        e,
        Expr::Variant { payload: Some(p), .. } if is_value_form(p)
    )
}

fn hint_name(n: &str) -> String {
    if n.starts_with("__builtin_") {
        String::new()
    } else {
        n.chars().take(8).collect()
    }
}

fn hint_pat(p: &Pat) -> String {
    match p {
        Pat::Var(n, _) => hint_name(n),
        Pat::Wild(_) => "_".into(),
    }
}

pub fn path_string(path: &[PathSeg]) -> String {
    use PathSeg::*;
    let mut out = String::from("$");
    for seg in &path[1..] {
        out.push('/');
        match seg {
            Root => out.push('r'),
            LamBody => out.push_str("lam_body"),
            AppFun => out.push_str("app_fun"),
            AppArg => out.push_str("app_arg"),
            LetRhs => out.push_str("let_rhs"),
            LetBody => out.push_str("let_body"),
            IfCond => out.push_str("if_cond"),
            IfThen => out.push_str("if_then"),
            IfElse => out.push_str("if_else"),
            RecordField(n) => {
                out.push_str("rec.");
                out.push_str(n);
            }
            ExtendBase => out.push_str("ext_base"),
            ExtendField(n) => {
                out.push_str("ext.");
                out.push_str(n);
            }
            FieldOf => out.push_str("field_of"),
            VariantPayload => out.push_str("var_payload"),
            MatchScrut => out.push_str("match_scrut"),
            MatchCase(n) => {
                out.push_str("case.");
                out.push_str(n);
            }
            CaseBody(n) => {
                out.push_str("case_body.");
                out.push_str(n);
            }
            AnnInner => out.push_str("ann"),
        }
    }
    out
}

fn describe_expr(e: &Expr, src: &str) -> (String, String) {
    let txt = |sp: Span| {
        let t = sp.text(src);
        if t.len() > 28 { format!("{}…", &t[..t.char_indices().nth(24).map(|(i, _)| i).unwrap_or(t.len())]) } else { t.to_string() }
    };
    match e {
        Expr::Lit(_, sp) => ("lit".into(), txt(*sp)),
        Expr::Var(n, _sp) => ("var".into(), n.clone()),
        Expr::Lam { .. } => ("lambda".into(), "fun".into()),
        Expr::App { .. } => ("app".into(), "application".into()),
        Expr::Let { rec, .. } => ("let".into(), if *rec { "let rec".into() } else { "let".into() }),
        Expr::If { .. } => ("if".into(), "if".into()),
        Expr::Record { .. } => ("record".into(), "record".into()),
        Expr::Extend { .. } => ("extend".into(), "record extension".into()),
        Expr::Field { name, .. } => ("field".into(), format!(".{}", name)),
        Expr::Variant { label, .. } => ("variant".into(), label.clone()),
        Expr::Match { .. } => ("match".into(), "match".into()),
        Expr::Ann { .. } => ("ann".into(), "annotation".into()),
    }
}

fn visit_collect(_e: &Expr, out: &mut Vec<NodeIdT>) {
    let _ = out;
}

fn direct_child_ids(
    expr: &Expr,
    ast_paths: &HashMap<NodeIdT, String>,
    parent_path: &[PathSeg],
) -> Vec<NodeIdT> {
    // Recover the ids assigned to immediate subexpressions by matching paths.
    let mut want: Vec<String> = Vec::new();
    let mut push_child = |seg: PathSeg| {
        let mut p = parent_path.to_vec();
        p.push(seg);
        want.push(path_string(&p));
    };
    match expr {
        Expr::Lit(..) | Expr::Var(..) => {}
        Expr::Lam { body, .. } => push_child(PathSeg::LamBody),
        Expr::App { .. } => {
            push_child(PathSeg::AppFun);
            push_child(PathSeg::AppArg);
        }
        Expr::Let { .. } => {
            push_child(PathSeg::LetRhs);
            push_child(PathSeg::LetBody);
        }
        Expr::If { .. } => {
            push_child(PathSeg::IfCond);
            push_child(PathSeg::IfThen);
            push_child(PathSeg::IfElse);
        }
        Expr::Record { fields, .. } => {
            for (n, _, _) in fields {
                push_child(PathSeg::RecordField(n.clone()));
            }
        }
        Expr::Extend { fields, .. } => {
            push_child(PathSeg::ExtendBase);
            for (n, _, _) in fields {
                push_child(PathSeg::ExtendField(n.clone()));
            }
        }
        Expr::Field { .. } => push_child(PathSeg::FieldOf),
        Expr::Variant { payload, .. } => {
            if payload.is_some() {
                push_child(PathSeg::VariantPayload);
            }
        }
        Expr::Match { cases, .. } => {
            push_child(PathSeg::MatchScrut);
            for (l, _, _, _) in cases {
                push_child(PathSeg::CaseBody(l.clone()));
            }
        }
        Expr::Ann { .. } => push_child(PathSeg::AnnInner),
    }
    let mut result = Vec::new();
    for w in want {
        if let Some((id, _)) = ast_paths.iter().find(|(_, p)| **p == w) {
            result.push(*id);
        }
    }
    result
}
