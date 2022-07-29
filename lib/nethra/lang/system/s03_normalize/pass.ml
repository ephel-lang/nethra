module Impl = struct
  open Preface.List.Functor
  open Nethra_lang_ast.Context.Hypothesis

  type 'a input = 'a Nethra_lang_ast.Context.Hypothesis.t
  type 'a output = 'a Nethra_lang_ast.Context.Hypothesis.t
  type 'a error = ('a option * string) list

  let check_free_vars hypothesis (name, term) =
    let bound_variables = Access.signatures hypothesis <&> fst in
    let free_variables = Term.freevars bound_variables term in
    match free_variables with
    | [] -> Result.Ok (name, term)
    | l -> Result.Error l

  let run h =
    let open Nethra_lang_ast.Context.Hypothesis in
    let _signatures = List.map (check_free_vars h) (Access.signatures h) in
    let _definitions = List.map (check_free_vars h) (Access.definitions h) in
    Result.Ok h
end