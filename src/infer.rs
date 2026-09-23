use std::collections::{BTreeMap, HashMap, HashSet};

use serde::Serialize;
use serde_json::Value;

use crate::parser::{AstNode, AstNodeData};
use crate::span::Span;
use crate::types::{
    render_view, NameMap, Origin, RowField, RowTail, Type, TypeView, UFEntry, UnionFind,
};

#[derive(Clone, Debug, Serialize)]
pub struct ConstraintRec {
    pub cid: String,
    pub role: String,
    pub node: String,
    pub status: String,
    pub cache_key: String,
    pub disabled: bool,
    pub detail: Value,
    pub text: String,
    pub error: Option<usize>,
}

#[derive(Clone, Debug, Serialize)]
pub struct StepRec {
    pub idx: usize,
    pub kind: String,
    pub cid: String,
    pub node: String,
    pub text: String,
    pub root_view: Option<TypeView>,
}

#[derive(Clone, Debug, Serialize)]
pub struct FieldErr {
    pub label: String,
    pub constraints: Vec<String>,
}

#[derive(Clone, Debug, Serialize)]
pub struct TypeErrorRec {
    pub id: usize,
    pub kind: String,
    pub message: String,
    pub span: Span,
    pub related: Vec<Span>,
    pub left: Option<String>,
    pub right: Option<String>,
    pub occurs: Option<String>,
    pub missing: Vec<FieldErr>,
    pub extra: Vec<FieldErr>,
    pub constraints: Vec<String>,
    pub cores: Vec<Vec<String>>,
}

#[derive(Clone, Debug, Serialize)]
pub struct SchemeView {
    pub id: String,
    pub quant: Vec<String>,
    pub body: TypeView,
    pub node: String,
}

#[derive(Clone, Debug, Serialize)]
pub struct NodeType {
    pub node: String,
    pub ty: TypeView,
    pub rendered: String,
    pub prov: Vec<String>,
    pub src: Vec<String>,
}

#[derive(Clone, Debug, Serialize)]
pub struct Analysis {
    pub ok: bool,
    pub nodes: Vec<AstNode>,
    pub root: String,
    pub constraints: Vec<ConstraintRec>,
    pub steps: Vec<StepRec>,
    pub errors: Vec<TypeErrorRec>,
    pub node_types: Vec<NodeType>,
    pub schemes: Vec<SchemeView>,
    pub final_type: Option<TypeView>,
    pub final_rendered: String,
    pub stats: AnalysisStats,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct AnalysisStats {
    pub constraints_total: usize,
    pub constraints_reused: usize,
    pub constraints_new: usize,
    pub steps: usize,
    pub errors: usize,
    pub generalized: usize,
    pub instantiated: usize,
}

// ---------------- unification failure ----------------

#[derive(Clone, Debug)]
enum UniErr {
    Occurs {
        var: u32,
        ty: Type,
        cid: String,
        node: String,
    },
    Clash {
        left: String,
        right: String,
        cid: String,
        node: String,
        prov: Vec<Origin>,
    },
    Row {
        is_record: bool,
        missing: Vec<(String, Vec<Origin>)>,
        extra: Vec<(String, Vec<Origin>)>,
        cid: String,
        node: String,
    },
}

pub struct ReuseInfo {
    pub reused: HashSet<String>,
}

pub struct InferOpts<'a> {
    pub disabled: &'a HashSet<String>,
    pub reuse: Option<&'a ReuseInfo>,
    pub cache_keys: &'a HashMap<String, String>,
    pub hunt: bool,
}

struct Scheme {
    quant: Vec<u32>,
    body: Type,
}

struct GenOrigin {
    label: String,
    node: String,
    kind: String,
}

pub struct Infer<'a> {
    nodes: &'a [AstNode],
    node_map: HashMap<String, &'a AstNode>,
    uf: UnionFind,
    names: NameMap,
    env: Vec<(String, String, Scheme)>, // (name, scheme cid, scheme)
    constraints: Vec<ConstraintRec>,
    steps: Vec<StepRec>,
    errors: Vec<TypeErrorRec>,
    schemes: Vec<SchemeView>,
    node_types: Vec<(String, Type)>,
    opts: InferOpts<'a>,
    inst_count: HashMap<String, usize>,
    hunt_signature: Option<Value>,
    generalized: usize,
    instantiated: usize,
    root_path: String,
    saved_schemes: HashMap<String, (Vec<u32>, Type)>,
}

impl<'a> Infer<'a> {
    pub fn analyze(
        nodes: &'a [AstNode],
        root: &str,
        opts: InferOpts<'a>,
    ) -> (Analysis, Option<Value>) {
        let node_map: HashMap<_, _> = nodes.iter().map(|n| (n.path.clone(), n)).collect();
        let mut inf = Infer {
            nodes,
            node_map,
            uf: UnionFind::new(),
            names: NameMap::new(),
            env: Vec::new(),
            constraints: Vec::new(),
            steps: Vec::new(),
            errors: Vec::new(),
            schemes: Vec::new(),
            node_types: Vec::new(),
            opts,
            inst_count: HashMap::new(),
            hunt_signature: None,
            generalized: 0,
            instantiated: 0,
            root_path: root.to_string(),
            saved_schemes: HashMap::new(),
        };
        let ty = inf.gen_node(root, 0);
        inf.finish(ty)
    }

    fn node(&self, path: &str) -> &AstNode {
        self.node_map.get(path).copied().unwrap_or_else(|| {
            panic!("missing ast node {}", path);
        })
    }

    fn origin(&self, label: String, node: &str, kind: &str, cid: String) -> Origin {
        Origin {
            label,
            node: Some(node.to_string()),
            constraint: cid,
            kind: kind.to_string(),
        }
    }

    fn fresh(&mut self, rank: u32, label: String, node: &str, kind: &str, cid: String) -> Type {
        self.uf.fresh(rank, self.origin(label, node, kind, cid))
    }

    fn named_fresh(&mut self, rank: u32, name: &str, label: String, node: &str, kind: &str, cid: String) -> Type {
        let t = self.fresh(rank, label, node, kind, cid);
        if let Type::Var(v) = t {
            self.names.name_var(v, name);
        }
        t
    }

    fn snapshot_root(&self) -> Option<TypeView> {
        None
    }

    fn record_step(&mut self, kind: &str, cid: &str, node: &str, text: String, root_ty: Option<&Type>) {
        if self.opts.hunt {
            return;
        }
        let root_view = root_ty.map(|t| {
            let mut tmp_names = NameMap::new();
            clone_names(&self.names, &mut tmp_names);
            let mut uf = snapshot_uf(&self.uf);
            uf.view(t, &tmp_names)
        });
        self.steps.push(StepRec {
            idx: self.steps.len(),
            kind: kind.to_string(),
            cid: cid.to_string(),
            node: node.to_string(),
            text,
            root_view,
        });
    }

    fn cache_key(&self, node: &str, role: &str) -> String {
        let base = self.opts.cache_keys.get(node).cloned().unwrap_or_default();
        format!("{}::{}", base, role)
    }

    fn emit_status(&self, cid: &str) -> String {
        if let Some(r) = &self.opts.reuse {
            if r.reused.contains(cid) {
                return "reused".into();
            }
        }
        "new"
    }

    fn emit_eq(
        &mut self,
        node: &str,
        role: &str,
        a: &Type,
        b: &Type,
        strict: bool,
        text: String,
    ) {
        let cid = format!("{}#{}", node, role);
        let disabled = self.opts.disabled.contains(&cid);
        let detail = serde_json::json!({
            "a": a,
            "b": b,
            "strict": strict,
        });
        let status = self.emit_status(&cid);
        let mut rec = ConstraintRec {
            cid: cid.clone(),
            role: role.to_string(),
            node: node.to_string(),
            status,
            cache_key: self.cache_key(node, role),
            disabled,
            detail,
            text: String::new(),
            error: None,
        };
        if disabled {
            self.snapshot_step_for(&cid, node, "skip", &text, None, a);
            rec.text = format!("{}（已禁用）", text);
            self.constraints.push(rec);
            return;
        }
        let result = self.unify(a, b, &cid, node, strict);
        let root_ty = self.resolve_node_type(&self.root_path.clone());
        if let Err(e) = result {
            self.record_error(e, &cid, node, Some(&mut rec));
        }
        rec.text = text;
        self.constraints.push(rec);
    }

    fn snapshot_step_for(
        &mut self,
        cid: &str,
        node: &str,
        kind: &str,
        text: &str,
        root_override: Option<&Type>,
        _ty: &Type,
    ) {
        let root_ty = root_override
            .cloned()
            .or_else(|| self.resolve_node_type(&self.root_path.clone()));
        self.record_step(kind, cid, node, text.to_string(), root_ty.as_ref());
    }

    fn resolve_node_type(&self, path: &str) -> Option<Type> {
        self.node_types.iter().rev().find(|(p, _)| p == path).map(|(_, t)| t.clone())
    }

    fn emit_gen(&mut self, node: &str, scheme_id: &str, body: &Type, quant: &[u32], restricted: bool) {
        let cid = format!("{}#gen", node);
        let rendered = {
            let mut uf = snapshot_uf(&self.uf);
            let mut names = NameMap::new();
            clone_names(&self.names, &mut names);
            uf.render(body, &names)
        };
        let text = if quant.is_empty() {
            format!("泛化 {}（{}，未引入类型变量）", scheme_id, rendered)
        } else {
            let qnames: Vec<String> = quant.iter().map(|v| self.names.name_of(*v)).collect();
            format!(
                "泛化 {} : forall {}. {}{}",
                scheme_id,
                qnames.join(" "),
                rendered,
                if restricted { "（value restriction：仅限危险变量外的变量）" } else { "" }
            )
        };
        let detail = serde_json::json!({"scheme": scheme_id, "quant": quant, "restricted": restricted});
        let status = self.emit_status(&cid);
        self.constraints.push(ConstraintRec {
            cid: cid.clone(),
            role: "gen".into(),
            node: node.to_string(),
            status,
            cache_key: self.cache_key(node, "gen"),
            disabled: false,
            detail,
            text: text.clone(),
            error: None,
        });
        let root_ty = self.resolve_node_type(&self.root_path.clone());
        self.record_step("generalize", &cid, node, text, root_ty.as_ref());
    }

    fn emit_inst(&mut self, node: &str, scheme_id: &str, from: &Type, to: &Type) {
        let n = self.inst_count.entry(node.to_string()).or_insert(0);
        *n += 1;
        let role = format!("inst{}", *n);
        let cid = format!("{}#{}", node, role);
        let text = format!("实例化方案 {}（在 {}）", scheme_id, node);
        let detail = serde_json::json!({"scheme": scheme_id, "from": from, "to": to});
        let status = self.emit_status(&cid);
        self.constraints.push(ConstraintRec {
            cid: cid.clone(),
            role,
            node: node.to_string(),
            status,
            cache_key: self.cache_key(node, &format!("inst{}", *n)),
            disabled: false,
            detail,
            text: text.clone(),
            error: None,
        });
        let root_ty = self.resolve_node_type(&self.root_path.clone());
        self.record_step("instantiate", &cid, node, text, root_ty.as_ref());
    }
}

fn clone_names(src: &NameMap, dst: &mut NameMap) {
    // serialize names through a temporary UF view is complex; simplest:
    // rebuild from public maps via accessors below.
    for (k, v) in src.all_named() {
        dst.name_var(k, &v);
    }
}

fn snapshot_uf(uf: &UnionFind) -> UnionFind {
    let entries = uf
        .entries
        .iter()
        .map(|e| UFEntry {
            rank: e.rank,
            root: e.root.clone(),
            origins: e.origins.clone(),
        })
        .collect();
    UnionFind { entries }
}
