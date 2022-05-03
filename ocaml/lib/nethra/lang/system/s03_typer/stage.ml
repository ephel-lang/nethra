module Impl = struct
  type 'a input = 'a Nethra_lang_ast.Context.Hypothesis.t

  type 'a output =
    'a Nethra_lang_ast.Context.Hypothesis.t
    * (string * 'a Nethra_lang_ast.Proof.t option) list

  module rec TypeChecker : Specs.Checker =
    Checker.Impl
      (struct
        let type_in_type = true
      end)
      (Infer.Impl (TypeChecker))

  let type_check h (id, exp) =
    let open Nethra_lang_ast.Context.Hypothesis in
    match Access.get_signature h id with
    | None -> (id, None)
    | Some spec -> (id, Some TypeChecker.(h |- exp <= spec))

  let run h =
    let open Nethra_lang_ast.Context.Hypothesis in
    Result.Ok (h, List.map (type_check h) (Access.definitions h))
end
