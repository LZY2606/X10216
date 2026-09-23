//! 统一：occurs-check、结构统一、行多态（缺字段/余字段分开报告）。

use crate::solver::arena::{Arena, Origin};
use crate::types::{Content, Kind, RowKind, TyId};

#[derive(Clone, Debug, PartialEq)]
pub enum UnifyError {
    Occurs {
        var: TyId,
        var_note: String,
        head: &'static str,
        inside_note: String,
    },
    Mismatch {
        left: &'static str,
        right: &'static str,
    },
    KindMismatch {
        left: &'static str,
        right: &'static str,
    },
    RowMismatch {
        kind: RowKind,
        missing: Vec<String>,
        extra: Vec<String>,
        closed_side: ClosedSide,
    },
    DuplicateLabel {
        kind: RowKind,
        label: String,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ClosedSide {
    Left,
    Right,
}

#[derive(Clone, Debug, PartialEq)]
pub enum UEvent {
    Resolve {
        var: TyId,
        into: &'static str,
    },
    UnifyStruct {
        head: &'static str,
    },
    UnifyRow {
        kind: RowKind,
        labels: Vec<String>,
    },
    LinkRow {
        kind: RowKind,
        labels: Vec<String>,
        tail_open: bool,
    },
    OccursFail {
        var: TyId,
        head: &'static str,
    },
    Mismatch {
        left: &'static str,
        right: &'static str,
    },
    RowFail {
        kind: RowKind,
        missing: Vec<String>,
        extra: Vec<String>,
    },
}

/// 压平后的行（所有权数据）。
#[derive(Clone, Debug)]
pub struct FlatRow {
    pub kind: RowKind,
    pub fields: Vec<(String, TyId)>,
    /// 链末端的开放行变量；None 表示闭合。
    pub tail: Option<TyId>,
}

pub fn flatten_row(arena: &mut Arena, row: TyId) -> Option<FlatRow> {
    let mut fields: Vec<(String, TyId)> = Vec::new();
    let mut cur = arena.find(row);
    let mut kind = None;
    let mut tail = None;
    for _ in 0..4096 {
        let c = arena.content(cur).clone();
        match c {
            Content::Row {
                kind: rk,
                fields: fs,
                tail: t,
            } => {
                kind = Some(rk);
                for f in fs {
                    fields.push(f);
                }
                match t {
                    None => {
                        tail = None;
                        break;
                    }
                    Some(next) => {
                        let n = arena.find(next);
                        if n == cur {
                            // 自环尾巴：视为开放
                            tail = Some(next);
                            break;
                        }
                        match arena.content(n) {
                            Content::Row { .. } => {
                                cur = n;
                                continue;
                            }
                            Content::Flex { .. } => {
                                tail = Some(n);
                                break;
                            }
                            _ => {
                                tail = Some(n);
                                break;
                            }
                        }
                    }
                }
            }
            Content::Flex { .. } => {
                tail = Some(cur);
                break;
            }
            _ => return None,
        }
    }
    Some(FlatRow {
        kind: kind?,
        fields,
        tail,
    })
}

/// occurs-check：var 是否（通过结构或开放行尾）出现在 root 中。
fn occurs(arena: &Arena, var: TyId, root: TyId, seen: &mut Vec<u32>) -> bool {
    let r = arena.find_imm(root);
    if r == var {
        return true;
    }
    if seen.contains(&r.0) {
        return false;
    }
    seen.push(r.0);
    match arena.content(r) {
        Content::Flex { .. } | Content::Int | Content::Bool => false,
        Content::Arrow { params, ret } => {
            params.iter().any(|p| occurs(arena, var, *p, seen))
                || occurs(arena, var, *ret, seen)
        }
        Content::Row { fields, tail, .. } => {
            fields.iter().any(|(_, t)| occurs(arena, var, *t, seen))
                || tail.map(|t| occurs(arena, var, t, seen)).unwrap_or(false)
        }
    }
}

fn origin_note(arena: &Arena, t: TyId) -> String {
    arena.origin[t.0 as usize].note.clone()
}

/// 统一两个类型；产生的微观事件写入 events。
pub fn unify(
    arena: &mut Arena,
    a: TyId,
    b: TyId,
    events: &mut Vec<UEvent>,
) -> Result<(), UnifyError> {
    let ra = arena.find(a);
    let rb = arena.find(b);
    if ra == rb {
        return Ok(());
    }
    let ca = arena.content(ra).clone();
    let cb = arena.content(rb).clone();
    match (&ca, &cb) {
        (Content::Flex { .. }, _) => {
            if occurs(arena, rb, ra, &mut Vec::new()) {
                events.push(UEvent::OccursFail {
                    var: ra,
                    head: cb.head_name(),
                });
                return Err(UnifyError::Occurs {
                    var: ra,
                    var_note: origin_note(arena, ra),
                    head: cb.head_name(),
                    inside_note: origin_note(arena, rb),
                });
            }
            // 统一层级取 min
            let la = level_of(arena, ra);
            let lb = level_of(arena, rb);
            let nl = la.min(lb);
            set_level(arena, rb, nl);
            arena.link_to(ra, rb);
            events.push(UEvent::Resolve {
                var: ra,
                into: cb.head_name(),
            });
            Ok(())
        }
        (_, Content::Flex { .. }) => {
            if occurs(arena, ra, rb, &mut Vec::new()) {
                events.push(UEvent::OccursFail {
                    var: rb,
                    head: ca.head_name(),
                });
                return Err(UnifyError::Occurs {
                    var: rb,
                    var_note: origin_note(arena, rb),
                    head: ca.head_name(),
                    inside_note: origin_note(arena, ra),
                });
            }
            let la = level_of(arena, ra);
            let lb = level_of(arena, rb);
            set_level(arena, ra, la.min(lb));
            arena.link_to(rb, ra);
            events.push(UEvent::Resolve {
                var: rb,
                into: ca.head_name(),
            });
            Ok(())
        }
        (Content::Int, Content::Int) | (Content::Bool, Content::Bool) => Ok(()),
        (Content::Arrow { params: pa, ret: ra2 }, Content::Arrow { params: pb, ret: rb2 }) => {
            if pa.len() != pb.len() {
                events.push(UEvent::Mismatch {
                    left: "arrow",
                    right: "arrow",
                });
                return Err(UnifyError::Mismatch {
                    left: "arrow",
                    right: "arrow-arity",
                });
            }
            events.push(UEvent::UnifyStruct { head: "->" });
            for (x, y) in pa.iter().zip(pb.iter()) {
                unify(arena, *x, *y, events)?;
            }
            unify(arena, *ra2, *rb2, events)
        }
        (Content::Row { kind: ka, .. }, Content::Row { kind: kb, .. }) if ka == kb => {
            unify_rows(arena, ra, rb, *ka, events)
        }
        _ => {
            let (ha, hb) = (ca.head_name(), cb.head_name());
            if ha != hb {
                events.push(UEvent::Mismatch {
                    left: ha,
                    right: hb,
                });
                return Err(UnifyError::Mismatch {
                    left: ha,
                    right: hb,
                });
            }
            // record vs variant
            events.push(UEvent::Mismatch {
                left: ha,
                right: hb,
            });
            Err(UnifyError::KindMismatch {
                left: ha,
                right: hb,
            })
        }
    }
}

fn level_of(arena: &Arena, t: TyId) -> u32 {
    match arena.content(t) {
        Content::Flex { level, .. } => *level,
        _ => u32::MAX,
    }
}

/// 更新具体结构内部未解析变量的层级（泛化前的 demotion 用）。
fn set_level(arena: &mut Arena, root: TyId, level: u32) {
    // 仅当代表为 flex 时需要调整自身
    if let Content::Flex { level: l, generic } = arena.content(root).clone() {
        if l > level {
            arena.nodes[root.0 as usize].content = Content::Flex {
                level,
                generic,
            };
        }
    }
}

fn unify_rows(
    arena: &mut Arena,
    ra: TyId,
    rb: TyId,
    kind: RowKind,
    events: &mut Vec<UEvent>,
) -> Result<(), UnifyError> {
    let fa = flatten_row(arena, ra).expect("row a");
    let fb = flatten_row(arena, rb).expect("row b");

    // 重复标签检测（压平后同标签出现多次）
    for (i, (k, _)) in fa.fields.iter().enumerate() {
        if fa.fields[..i].iter().any(|(k2, _)| k2 == k) {
            return Err(UnifyError::DuplicateLabel { kind, label: k.clone() });
        }
    }
    for (i, (k, _)) in fb.fields.iter().enumerate() {
        if fb.fields[..i].iter().any(|(k2, _)| k2 == k) {
            return Err(UnifyError::DuplicateLabel { kind, label: k.clone() });
        }
    }

    let mut merged: Vec<(String, TyId, TyId)> = Vec::new();
    let mut only_left: Vec<(String, TyId)> = Vec::new();
    let mut only_right: Vec<(String, TyId)> = Vec::new();
    for (k, va) in &fa.fields {
        match fb.fields.iter().find(|(k2, _)| k2 == k) {
            Some((_, vb)) => merged.push((k.clone(), *va, *vb)),
            None => only_left.push((k.clone(), *va)),
        }
    }
    for (k, vb) in &fb.fields {
        if !fa.fields.iter().any(|(k2, _)| k2 == k) {
            only_right.push((k.clone(), *vb));
        }
    }

    events.push(UEvent::UnifyRow {
        kind,
        labels: merged.iter().map(|(k, _, _)| k.clone()).collect(),
    });

    // 先统一共有字段；任一字段冲突都属于整个行冲突
    for (_, va, vb) in &merged {
        unify(arena, *va, *vb, events)?;
    }

    let left_open = fa.tail.is_some();
    let right_open = fb.tail.is_some();

    if !only_left.is_empty() && !only_right.is_empty() {
        // 两侧都有独占标签：即使都开放也无法统一（保留差异并区分缺/余）
        fail_row(kind, &only_left, &only_right, events, ClosedSide::Right)?;
    }

    if left_open && right_open {
        // 两开放行：共有字段已统一；把两侧 tail 变量统一，
        // 独占字段并入另一侧（通过构造新的残差行）。
        let ta = fa.tail.unwrap();
        let tb = fb.tail.unwrap();
        // 把左侧独占字段加入 b 侧 tail（新行指向 tb）
        if !only_left.is_empty() {
            let fields = only_left.clone();
            let nr = arena.make(
                crate::types::TyNode {
                    content: Content::Row {
                        kind,
                        fields,
                        tail: Some(tb),
                    },
                    kind: Kind::Row,
                },
                Origin::new("行多态残差（并入右侧）"),
            );
            unify(arena, ta, nr, events)?;
        } else if !only_right.is_empty() {
            let fields = only_right.clone();
            let nr = arena.make(
                crate::types::TyNode {
                    content: Content::Row {
                        kind,
                        fields,
                        tail: Some(ta),
                    },
                    kind: Kind::Row,
                },
                Origin::new("行多态残差（并入左侧）"),
            );
            unify(arena, tb, nr, events)?;
        } else {
            unify(arena, ta, tb, events)?;
        }
        events.push(UEvent::LinkRow {
            kind,
            labels: merged.iter().map(|(k, _, _)| k.clone()).collect(),
            tail_open: true,
        });
        return Ok(());
    }

    if left_open && !right_open {
        // b 闭合：a 缺少 b 的字段？b 所有字段必须出现在 a 中。
        // only_right 非空 => a 缺少；only_left => b 多余（闭合不能接纳）。
        if !only_left.is_empty() || !only_right.is_empty() {
            fail_row(kind, &only_left, &only_right, events, ClosedSide::Right)?;
        }
        let ta = fa.tail.unwrap();
        // b 已无更多字段：a 的 tail 统一为「空闭合行」
        let empty = arena.make(
            crate::types::TyNode {
                content: Content::Row {
                    kind,
                    fields: vec![],
                    tail: None,
                },
                kind: Kind::Row,
            },
            Origin::new("闭合空行"),
        );
        unify(arena, ta, empty, events)?;
        return Ok(());
    }

    if !left_open && right_open {
        if !only_left.is_empty() || !only_right.is_empty() {
            fail_row(kind, &only_left, &only_right, events, ClosedSide::Left)?;
        }
        let tb = fb.tail.unwrap();
        let empty = arena.make(
            crate::types::TyNode {
                content: Content::Row {
                    kind,
                    fields: vec![],
                    tail: None,
                },
                kind: Kind::Row,
            },
            Origin::new("闭合空行"),
        );
        unify(arena, tb, empty, events)?;
        return Ok(());
    }

    // 两闭合：字段集合必须一致
    if !only_left.is_empty() || !only_right.is_empty() {
        fail_row(kind, &only_left, &only_right, events, ClosedSide::Right)?;
    }
    events.push(UEvent::LinkRow {
        kind,
        labels: merged.iter().map(|(k, _, _)| k.clone()).collect(),
        tail_open: false,
    });
    Ok(())
}

fn fail_row(
    kind: RowKind,
    only_left: &[(String, TyId)],
    only_right: &[(String, TyId)],
    events: &mut Vec<UEvent>,
    _closed_side: ClosedSide,
) -> Result<(), UnifyError> {
    let missing = only_left.iter().map(|(k, _)| k.clone()).collect();
    let extra = only_right.iter().map(|(k, _)| k.clone()).collect();
    let _ = only_left;
    let _ = only_right;
    events.push(UEvent::RowFail {
        kind,
        missing,
        extra,
    });
    Err(UnifyError::RowMismatch {
        kind,
        missing,
        extra,
        closed_side: _closed_side,
    })
}
