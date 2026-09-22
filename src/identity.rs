use crate::ast::*;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct AstDiff {
    pub old_to_new: HashMap<usize, usize>,
    pub new_to_old: HashMap<usize, usize>,
    pub added: Vec<usize>,
    pub removed: Vec<usize>,
}

pub fn stabilize(old: Option<&Program>, parsed: &RawProgram) -> (Program, AstDiff) {
    let mut out = Program { nodes: HashMap::new(), roots: parsed.roots.clone() };
    let mut diff = AstDiff::default();
    let mut stable_counter = old.and_then(|p| p.nodes.keys().copied().max()).map(|x| x + 1).unwrap_or(0);
    let mut raw_to_stable: HashMap<usize, usize> = HashMap::new();

    if let Some(old) = old {
        let mut binder_old_to_new: HashMap<usize, usize> = HashMap::new();
        let mut root_pairs = vec![];
        let mut used_old = vec![false; old.roots.len()];
        let mut used_new = vec![false; parsed.roots.len()];
        for (ni, nr) in parsed.roots.iter().enumerate() {
            let mut best = None;
            for (oi, or) in old.roots.iter().enumerate() {
                if used_old[oi] || !roots_compatible(or, nr) { continue; }
                let score = root_score(old, oi, parsed, ni);
                if best.map(|(_, b)| score > b).unwrap_or(true) { best = Some((oi, score)); }
            }
            if let Some((oi, _)) = best {
                used_old[oi] = true;
                used_new[ni] = true;
                root_pairs.push((oi, ni));
            }
        }
        for (oi, ni) in root_pairs {
            pair_roots(old, parsed, oi, ni, &mut binder_old_to_new, &mut root_pairs.len().to_string());
        }
        for (oi, oroot) in old.roots.iter().enumerate() {
            if !used_old[oi] {
                for id in root_children(oroot) {
                    collect_raw(old, id, &mut diff.removed);
                }
            }
        }
        for (ni, nroot) in parsed.roots.iter().enumerate() {
            if !used_new[ni] {
                for id in root_children(nroot) {
                    assign_new(parsed, id, &mut out, &mut raw_to_stable, &mut diff.added, &mut stable_counter);
                }
            }
        }

        fn pair_roots(old: &Program, new: &RawProgram, oi: usize, ni: usize, bm: &mut HashMap<usize, usize>, _ctx: &str) {
            let ov = old.roots[oi].clone();
            let nv = new.roots[ni].clone();
            match (&ov.kind, &nv.kind) {
                (RootKind::Let { value: ov, body: ob, .. }, RootKind::Let { value: nv, body: nb, .. }) => {
                    seed_binder(old, new, *ov, *nv, bm);
                    match_pair(old, new, *ov, *nv, bm);
                    if let (Some(ob), Some(nb)) = (ob, nb) { match_pair(old, new, *ob, *nb, bm); }
                }
                (RootKind::Expr(a), RootKind::Expr(b)) => match_pair(old, new, *a, *b, bm),
                _ => {}
            }
        }
    }

    for (rid, node) in parsed.nodes.iter() {
        if !raw_to_stable.contains_key(rid) {
            assign_new(parsed, *rid, &mut out, &mut raw_to_stable, &mut diff.added, &mut stable_counter);
        }
    }
    for (&raw, &stable) in &raw_to_stable {
        let n = &parsed.nodes[&raw];
        let children = n.children.iter().map(|(role, child)| (role.clone(), raw_to_stable[child])).collect();
        out.nodes.insert(stable, Node { id: stable, kind: n.kind.clone(), span: n.span, children });
        if let Some(old_id) = diff.new_to_old.get(&stable).copied() { diff.old_to_new.insert(old_id, stable); }
    }
    diff.added.sort();
    diff.removed.sort();
    (out, diff)
}

fn collect_raw(p: &Program, id: usize, out: &mut Vec<usize>) {
    if !out.contains(&id) {
        out.push(id);
        for (_, c) in node_children(p, id) { collect_raw(p, c, out); }
    }
}

fn assign_new(p: &RawProgram, id: usize, out: &mut Program, map: &mut HashMap<usize, usize>, added: &mut Vec<usize>, next: &mut usize) {
    if map.contains_key(&id) { return; }
    for (_, c) in raw_node_children(p, id) { assign_new(p, c, out, map, added, next); }
    let stable = *next;
    *next += 1;
    map.insert(id, stable);
    added.push(stable);
}

fn seed_binder(old: &Program, new: &RawProgram, o: usize, n: usize, bm: &mut HashMap<usize, usize>) {
    let on = &old.nodes[&o];
    let nn = &new.nodes[&n];
    if on.kind.discriminant() == nn.kind.discriminant() && matches!(on.kind, Kind::Abs { .. } | Kind::Let { .. } | Kind::MatchArm { .. }) {
        if let (Some(ob), Some(nb)) = (on.child("binder"), nn.child("binder")) { bm.insert(ob, nb); }
    }
}

fn roots_compatible(a: &RawRoot, b: &RawRoot) -> bool {
    matches!((&a.kind, &b.kind), (RootKind::Let { .. }, RootKind::Let { .. }) | (RootKind::Expr(_), RootKind::Expr(_)))
}

fn root_score(old: &Program, oi: usize, new: &RawProgram, ni: usize) -> usize {
    match (&old.roots[oi].kind, &new.roots[ni].kind) {
        (RootKind::Let { value: a, body: ab, .. }, RootKind::Let { value: b, body: bb, .. }) => {
            node_score(old, *a, new, *b) + ab.zip(*bb).map(|(a,b)| node_score(old,a,new,b)).unwrap_or(0)
        }
        (RootKind::Expr(a), RootKind::Expr(b)) => node_score(old,*a,new,*b),
        _ => 0,
    }
}

fn node_score(old: &Program, o: usize, new: &RawProgram, n: usize) -> usize {
    let on = &old.nodes[&o];
    let nn = &new.nodes[&n];
    if on.kind.discriminant() != nn.kind.discriminant() || on.kind.label() != nn.kind.label() { return 0; }
    1 + on.children.iter().map(|(role, oc)| {
        let opts = nn.children.iter().filter(|(r,_)| r == role).map(|(_, nc)| node_score(old,*oc,new,*nc)).max().unwrap_or(0);
        opts
    }).sum::<usize>()
}

fn vars_compatible(a: &Kind, b: &Kind, bm: &HashMap<usize, usize>) -> bool {
    match (a,b) {
        (Kind::Var { binder: Some(x), .. }, Kind::Var { binder: Some(y), .. }) => bm.get(x) == Some(y),
        (Kind::Var { binder: None, name: x, .. }, Kind::Var { binder: None, name: y, .. }) => x == y,
        _ => false,
    }
}

fn can_match(old: &Program, new: &RawProgram, o: usize, n: usize, bm: &HashMap<usize, usize>) -> bool {
    let a = &old.nodes[&o].kind;
    let b = &new.nodes[&n].kind;
    if a.discriminant() != b.discriminant() || a.label() != b.label() { return false; }
    if matches!(a, Kind::Var { .. }) { vars_compatible(a,b,bm) } else { true }
}

fn match_pair(old: &Program, new: &RawProgram, o: usize, n: usize, bm: &mut HashMap<usize, usize>) -> bool {
    if !can_match(old,new,o,n,bm) { return false; }
    seed_binder(old,new,o,n,bm);
    let on = &old.nodes[&o];
    let nn = &new.nodes[&n];
    let mut roles: Vec<_> = on.children.iter().map(|(r,_)| r).collect();
    roles.extend(nn.children.iter().map(|(r,_)| r));
    roles.sort();
    roles.dedup();
    for role in roles {
        let oc: Vec<_> = on.children.iter().filter(|(r,_)| r == role).map(|(_,id)| *id).collect();
        let nc: Vec<_> = nn.children.iter().filter(|(r,_)| r == role).map(|(_,id)| *id).collect();
        if oc.len() == 1 && nc.len() == 1 {
            let _ = match_pair(old,new,oc[0],nc[0],bm);
        } else {
            let mut choices = vec![None; nc.len()];
            let mut used = vec![false; oc.len()];
            for (ni, &nid) in nc.iter().enumerate() {
                let mut best = None;
                for (oi,&oid) in oc.iter().enumerate() {
                    if used[oi] || !can_match(old,new,oid,nid,bm) { continue; }
                    let score = node_score(old,oid,new,nid);
                    if best.map(|(_,b)| score > b).unwrap_or(true) { best = Some((oi,score)); }
                }
                if let Some((oi,_)) = best { used[oi] = true; choices[ni] = Some(oc[oi]); }
            }
            for (nid, oid) in choices.into_iter().zip(nc).filter_map(|(o,n)| o.map(|o| (n,o))) {
                let _ = match_pair(old,new,oid,nid,bm);
            }
        }
    }
    true
}
