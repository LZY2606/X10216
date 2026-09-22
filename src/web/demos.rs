//! Built-in demonstration programs (A and B versions).

pub const DEMOS: &[(&str, &str, &str, &str)] = &[
    (
        "let-poly",
        "let 多态",
        // A: classic let-polymorphic identity used at two types
        "let id = fun x -> x in\n(id 1) + 1",
        // B: rename and reuse; also apply to a record
        "let identity = fun z -> z in\n(identity { a = 2 }).a",
    ),
    (
        "infinite",
        "无限类型 (occurs check)",
        // A: self-application yields an infinite type
        "fun f -> f f",
        // B: fixed by applying f to a constant instead
        "fun f -> f 1",
    ),
    (
        "value-restriction",
        "value restriction",
        // A: eta-style value, generalizable
        "let v = fun x -> x in v 1",
        // B: non-generalizable application: same variable used at int and bool
        "let r = (fun x -> x) 1 in\nr == true",
    ),
    (
        "recursion",
        "递归绑定",
        // A: factorial
        "let rec fact = fun n -> if n == 0 then 1 else n * fact (n - 1) in\nfact 5",
        // B: rec used on booleans wrongly
        "let rec loop = fun n -> loop n in\nloop 3 == 4",
    ),
    (
        "row-extension",
        "行多态: 缺字段 / 多余字段",
        // A: project a missing field
        "let r = { a = 1 } in r.b",
        // B: open row function used with two record shapes
        "let get = fun r -> r.x in\nget { x = 1, y = 2 }",
    ),
    (
        "shadowing",
        "遮蔽 (shadowing)",
        // A: inner x shadows outer
        "let x = 1 in let x = true in x",
        // B: outer used in inner RHS
        "let x = 1 in let y = x + 1 in y",
    ),
    (
        "variants",
        "变体与 match",
        // A: closed match with an extra injected tag
        "let v = Some(1) in\nmatch v with\n| Some n -> n\n| None -> 0",
        // B: inject an unexpected tag
        "let v = Other(1) in\nmatch v with\n| Some n -> n\n| None -> 0",
    ),
    (
        "multiple-errors",
        "多个错误同时存在",
        // A: two independent clashes
        "(1 + true) == (\"x\" + 2)",
        // B: resolved one
        "(1 + 2) == (\"x\")",
    ),
    (
        "incremental",
        "增量编辑演示",
        "let twice = fun f x -> f (f x) in\ntwice (fun n -> n + 1) 0",
        // B: edit the body of the argument; outer constraints are reused
        "let twice = fun f x -> f (f x) in\ntwice (fun n -> n + 10) 0",
    ),
];
