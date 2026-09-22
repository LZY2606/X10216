use type_slice_bench::*;

fn main() {
    let cases = [
        ("id", "let id = fun x -> x in id 1"),
        ("id_poly_record", "let id = fun x -> x in { a = id 1, b = id true }"),
        ("occurs", "let f = fun x -> f x in f"),
        ("vr", "let r = (fun x -> x) 1 in { a = r, b = r true }"),
        ("rec_ok", "let rec loop = fun n -> loop n in loop 3"),
        ("rec_poly_fail", "let rec self = fun x -> self in { a = self 1, b = self true }"),
        ("record_ok", "{ a = 1, b = true }.a"),
        ("record_missing", "{ a = 1 }.b"),
        ("extra_field", "let r = { a = 1 } in r"),
        ("variant_ok", "match #Some 3 with #Some n => n | #None => 0"),
        ("variant_missing", "match #Some 3 with #None => 0"),
        ("variant_extra", "match (if true then #A 1 else #B 2) with #A n => n"),
        ("extend_ok", "let r = { a = 1, b = 2 } in { r with a = 3 }.b"),
        ("extend_missing", "let r = { a = 1 } in { r with b = 2 }.b"),
        ("mismatch", "1 true"),
        ("shadow", "let x = 1 in let x = true in x"),
        ("if_bad", "if 1 then 2 else 3"),
    ];
    for (name, src) in cases {
        match build_response(src, &[]) {
            Ok(b) => {
                let r = &b.response;
                let root = r.root_type.as_ref().map(|t| t.rendered.clone()).unwrap_or_else(|| "?".into());
                let errs: Vec<String> = r.errors.iter().map(|e| format!("{}[{}]", e.kind, e.message)).collect();
                println!("{:20} ok={:5} root={:30} errs={}", name, r.ok, root, errs.join(" ;; "));
            }
            Err(e) => println!("{:20} PARSE ERR {} {:?}", name, e.message, e.span),
        }
    }
}
