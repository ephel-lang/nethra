open Format

let check render hypothesis_render render_term prefix ppf
    (hypothesis, term, kind, proofs) =
  let () =
    fprintf ppf "%s%a ⊢ %a <= %a\n" prefix hypothesis_render hypothesis
      render_term term render_term kind
  in
  List.iter
    (fun p -> render hypothesis_render render_term (prefix ^ "|  ") ppf p)
    proofs

let infer render hypothesis_render render_term prefix ppf
    (hypothesis, term, kind, proofs) =
  let () =
    match kind with
    | Some kind ->
      fprintf ppf "%s%a ⊢ %a => %a\n" prefix hypothesis_render hypothesis
        render_term term render_term kind
    | None ->
      fprintf ppf "%s%a ⊢ %a => ?\n" prefix hypothesis_render hypothesis
        render_term term
  in
  List.iter
    (fun p -> render hypothesis_render render_term (prefix ^ "|  ") ppf p)
    proofs

let equivalent render hypothesis_render render_term prefix ppf
    (hypothesis, term, kind, proofs) =
  let () =
    fprintf ppf "%s%a ⊢ %a = %a\n" prefix hypothesis_render hypothesis
      render_term term render_term kind
  in
  List.iter
    (fun p -> render hypothesis_render render_term (prefix ^ "|  ") ppf p)
    proofs

let failure prefix ppf reason =
  fprintf ppf "%s%s \n" prefix
    (match reason with Some s -> "❌  (" ^ s ^ ")" | _ -> "❌")

let rec render hypothesis_render render_term prefix ppf p =
  Nethra_lang_ast.Proof.Destruct.fold
    ~check:(check render hypothesis_render render_term prefix ppf)
    ~infer:(infer render hypothesis_render render_term prefix ppf)
    ~equivalent:(equivalent render hypothesis_render render_term prefix ppf)
    ~failure:(failure prefix ppf) p

let render ?(term_render = Term.render) ppf p =
  render Hypothesis.render term_render "" ppf p