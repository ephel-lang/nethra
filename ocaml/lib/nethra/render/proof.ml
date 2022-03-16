let check render prefix ppf (term, kind, proofs) =
  let () =
    Format.fprintf ppf "%sΓ ⊢ %a <= %a\n" prefix Term.render term Term.render
      kind
  in
  List.iter (fun p -> render (prefix ^ "|  ") ppf p) proofs

let infer render prefix ppf (term, kind, proofs) =
  let () =
    match kind with
    | Some kind ->
      Format.fprintf ppf "%sΓ ⊢ %a => %a\n" prefix Term.render term Term.render
        kind
    | None -> Format.fprintf ppf "%sΓ ⊢ %a : ?\n" prefix Term.render term
  in
  List.iter (fun p -> render (prefix ^ "|  ") ppf p) proofs

let congruent render prefix ppf (term, kind, proofs) =
  let () =
    Format.fprintf ppf "%sΓ ⊢ %a = %a\n" prefix Term.render term Term.render
      kind
  in
  List.iter (fun p -> render (prefix ^ "|  ") ppf p) proofs

let failure prefix ppf reason =
  Format.fprintf ppf "%s%s \n" prefix
    (match reason with Some s -> s | _ -> "❌")

let rec render prefix ppf p =
  Nethra_ast.Proof.Catamorphism.fold ~check:(check render prefix ppf)
    ~infer:(infer render prefix ppf)
    ~congruent:(congruent render prefix ppf)
    ~failure:(failure prefix ppf) p

let render ppf p = render "" ppf p
