module Impl = struct
  type 'a input = 'a Nethra_lang_ast.Context.Hypothesis.t

  type 'a output =
    'a Nethra_lang_ast.Context.Hypothesis.t
    * (string * 'a Nethra_lang_ast.Proof.t option) list

  module Theory : Specs.Theory = struct
    let type_in_type = true
  end

  module rec TypeChecker : Specs.Checker =
    Checker.Impl (Theory) (Infer.Impl (Theory) (TypeChecker))

  let type_validate h (ident, exp) =
    let open Nethra_lang_ast.Term.Construct in
    (ident, Some TypeChecker.(h |- exp <= kind 0))

  let type_check h (ident, exp) =
    let open Preface.Option.Functor in
    let open Nethra_lang_ast.Context.Hypothesis in
    ( ident
    , Access.get_signature h ident
      <&> fun spec -> TypeChecker.(h |- exp <= spec) )

  let run h =
    let open Nethra_lang_ast.Context.Hypothesis in
    Result.Ok
      ( h
      , List.map (type_validate h) (Access.signatures h)
        @ List.map (type_check h) (Access.definitions h) )
end
