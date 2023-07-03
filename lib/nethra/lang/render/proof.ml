open Format

let render_list render hypothesis_render render_term ppf proofs =
  List.iter (fun p -> render hypothesis_render render_term ppf p) proofs

let check render hypothesis_render render_term ppf
    (hypothesis, term, kind, proofs) =
  fprintf ppf "@\n@[<v 2>%a ⊢ %a <= %a%a@]" hypothesis_render hypothesis
    render_term term render_term kind
    (render_list render hypothesis_render render_term)
    proofs

let infer render hypothesis_render render_term ppf
    (hypothesis, term, kind, proofs) =
  match kind with
  | Some kind ->
    fprintf ppf "@\n@[<v 2>%a ⊢ %a => %a%a@]" hypothesis_render hypothesis
      render_term term render_term kind
      (render_list render hypothesis_render render_term)
      proofs
  | None ->
    fprintf ppf "@\n@[<v 2>%a ⊢ %a => ?%a@]" hypothesis_render hypothesis
      render_term term
      (render_list render hypothesis_render render_term)
      proofs

let equivalent render hypothesis_render render_term ppf
    (hypothesis, term, kind, proofs) =
  fprintf ppf "@\n@[<v 2>%a ⊢ %a =?= %a%a@]" hypothesis_render hypothesis
    render_term term render_term kind
    (render_list render hypothesis_render render_term)
    proofs

let failure ppf reason =
  fprintf ppf " %s"
    (match reason with Some s -> "❌ (" ^ s ^ ")" | _ -> "❌ ")

let rec render hypothesis_render render_term ppf p =
  Nethra_lang_ast.Proof.Destruct.fold
    ~check:(check render hypothesis_render render_term ppf)
    ~infer:(infer render hypothesis_render render_term ppf)
    ~equivalent:(equivalent render hypothesis_render render_term ppf)
    ~failure:(failure ppf) p

let render ?(term_render = Term.render) ppf p =
  fprintf ppf "%a@\n" (render Hypothesis.render term_render) p
