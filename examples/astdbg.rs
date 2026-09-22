fn main() {
 let p=type_slice::parser::parse("let id = fn x => x\nid 1").unwrap();
 for (id, info) in &p.node_ids { if info.text=="id" || info.kind=="let" { println!("{id} {:?} {} binder={:?} ref={:?}", info.span, info.text, info.binder_id, info.ref_binder); }}
 println!("binders {:?}", p.binders);
 for s in &p.statements { println!("stmt {}", s.node_id); }
}
