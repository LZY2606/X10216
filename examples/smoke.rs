fn main() {
    for source in [
        "let id = fn x => x\nid 1\nid \"s\"",
        "let r = { a = 1 }\nr.b",
        "let f = fn x => fn y => x\nlet bad = f 1\nbad true",
        "let rec inf = fn x => inf x\ninf",
        "let v = ref 1\nv true",
        "{ a = 1, b = 2 }",
        "variant Some(1)",
        "match variant Some(1) with Some x => x",
        "let rec loop = loop\nloop",
    ] {
        println!("--- {source}");
        let analysis = type_slice::analyze_source(source, Default::default());
        println!("errors {}", analysis.errors.len());
        for e in &analysis.errors { println!("{} {} missing={:?} extra={:?} cores={:?}", e.kind, e.message, e.missing_fields, e.extra_fields, e.cores); }
        for (_, ty) in &analysis.final_types { println!("{}: {}", ty.name, ty.typ); }
    }
}
