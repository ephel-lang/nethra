open Format

let check render render_term prefix ppf (term, kind, proofs) =
  let () =
    fprintf ppf "%sΓ ⊢ %a <= %a\n" prefix render_term term render_term kind
  in
  List.iter (fun p -> render render_term (prefix ^ "|  ") ppf p) proofs

let infer render render_term prefix ppf (term, kind, proofs) =
  let () =
    match kind with
    | Some kind ->
      fprintf ppf "%sΓ ⊢ %a => %a\n" prefix render_term term render_term kind
    | None -> fprintf ppf "%sΓ ⊢ %a => ?\n" prefix Term.render term
  in
  List.iter (fun p -> render render_term (prefix ^ "|  ") ppf p) proofs

let equivalent render render_term prefix ppf (term, kind, proofs) =
  let () =
    fprintf ppf "%sΓ ⊢ %a = %a\n" prefix render_term term render_term kind
  in
  List.iter (fun p -> render render_term (prefix ^ "|  ") ppf p) proofs

let failure prefix ppf reason =
  fprintf ppf "%s%s \n" prefix
    (match reason with Some s -> "❌ " ^ s | _ -> "❌")

let rec render render_term prefix ppf p =
  Nethra_lang_ast.Proof.Destruct.fold
    ~check:(check render render_term prefix ppf)
    ~infer:(infer render render_term prefix ppf)
    ~equivalent:(equivalent render render_term prefix ppf)
    ~failure:(failure prefix ppf) p

let render ?(term = Term.render) ppf p = render term "" ppf p
