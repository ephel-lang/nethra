module Impl = struct
  type _ input = Nethra_toy_cst.Binding.t list

  type _ output =
    Nethra_syntax_source.Region.t Nethra_lang_ast.Context.Hypothesis.t

  type _ error = string

  let rec desugar_term r =
    let open Nethra_lang_ast.Term.Construct in
    let open Nethra_toy_cst.Term in
    function
    | Type l -> kind ~c:(Some r) l
    | Var l -> id ~c:(Some r) l
    | Literal (Int l) -> int ~c:(Some r) l
    | Literal (String l) -> string ~c:(Some r) l
    | Literal (Char l) -> char ~c:(Some r) l
    | Pi (n, t1, t2, b) ->
      pi ~c:(Some r) ~implicit:b n (desugar_localized t1)
        (desugar_localized t2)
    | Sigma (n, t1, t2) ->
      sigma ~c:(Some r) n (desugar_localized t1) (desugar_localized t2)
    | Lambda (n, t, b) ->
      lambda ~c:(Some r) ~implicit:b n (desugar_localized t)
    | Apply (t1, t2, b) ->
      apply ~c:(Some r) ~implicit:b (desugar_localized t1)
        (desugar_localized t2)
    | Let (n, t1, t2) ->
      apply ~c:(Some r) ~implicit:false
        (desugar_localized (Localized (Lambda (n, t2, false), r)))
        (desugar_localized t1)
    | Rec (n, k, t) ->
      mu ~c:(Some r) n (desugar_localized k) (desugar_localized t)
    | Sum (t1, t2) ->
      sum ~c:(Some r) (desugar_localized t1) (desugar_localized t2)
    | Case (t1, t2, t3) ->
      case ~c:(Some r) (desugar_localized t1) (desugar_localized t2)
        (desugar_localized t3)
    | Pair (t1, t2) ->
      pair ~c:(Some r) (desugar_localized t1) (desugar_localized t2)
    | BuildIn (Fst, t) -> fst ~c:(Some r) (desugar_localized t)
    | BuildIn (Snd, t) -> snd ~c:(Some r) (desugar_localized t)
    | BuildIn (Inl, t) -> inl ~c:(Some r) (desugar_localized t)
    | BuildIn (Inr, t) -> inr ~c:(Some r) (desugar_localized t)
    | BuildIn (Fold, t) -> fold ~c:(Some r) (desugar_localized t)
    | BuildIn (Unfold, t) -> unfold ~c:(Some r) (desugar_localized t)

  and desugar_localized =
    let open Nethra_toy_cst.Localized in
    function Localized (t, r) -> desugar_term r t

  let rec desugar hypothesis =
    let open Nethra_lang_ast.Context.Hypothesis.Access in
    let open Nethra_toy_cst.Binding in
    function
    | [] -> hypothesis
    | Signature (n, t) :: bindings ->
      desugar (add_signature hypothesis (n, desugar_localized t)) bindings
    | Definition (n, t) :: bindings ->
      desugar (add_definition hypothesis (n, desugar_localized t)) bindings

  let run l =
    let open Nethra_lang_ast.Context.Hypothesis.Construct in
    Result.Ok (desugar create l)
end
