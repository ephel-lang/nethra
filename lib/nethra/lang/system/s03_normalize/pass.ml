module Impl = struct
  open Preface.List.Functor
  open Nethra_lang_ast.Hypothesis

  type 'a input = 'a Nethra_lang_ast.Hypothesis.t
  type 'a output = 'a Nethra_lang_ast.Hypothesis.t
  type 'a error = ('a option * string) list

  let check_free_vars hypothesis (_, term) =
    let bound_variables = Access.signatures hypothesis <&> fst in
    Term.freevars bound_variables term

  let run h =
    let open Nethra_lang_ast.Hypothesis in
    let open Preface.List.Monad in
    let signatures = List.map (check_free_vars h) (Access.signatures h)
    and definitions = List.map (check_free_vars h) (Access.definitions h) in
    match join signatures @ join definitions with
    | [] -> Result.Ok h
    | l -> Result.Error l
end