module Impl = struct
  open Stdlib.Fun
  open Preface.Option.Functor
  open Preface.Option.Foldable

  type 'a input = 'a Nethra_lang_ast.Hypothesis.t

  type 'a output =
    'a Nethra_lang_ast.Hypothesis.t
    * (string * 'a Nethra_lang_ast.Proof.t option) list

  type _ error = string

  module Theory : Specs.Theory = struct
    let type_in_type = true
  end

  module rec TypeInfer : Specs.Infer =
    Infer.Impl (Theory) (Checker.Impl (Theory) (TypeInfer))

  module rec TypeChecker : Specs.Checker =
    Checker.Impl (Theory) (Infer.Impl (Theory) (TypeChecker))

  let type_validate h (ident, exp) =
    let open Nethra_lang_ast.Term.Construct in
    let open Nethra_lang_ast.Proof.Construct in
    let open Nethra_lang_ast.Proof.Destruct in
    let proof = TypeInfer.(h |- exp => ()) in
    let term = get_type proof in
    ( ident
    , Some
        (infer h exp term
           [
             proof
           ; fold_right const
               (term <&> fun term -> TypeChecker.(h |- term <= kind 1))
               (failure None)
           ] ) )

  let type_check h (ident, exp) =
    let open Preface.Option.Functor in
    let open Nethra_lang_ast.Hypothesis in
    ( ident
    , Access.get_signature h ident
      <&> fun spec -> TypeChecker.(h |- exp <= spec) )

  let run h =
    let open Nethra_lang_ast.Hypothesis in
    Result.Ok
      ( h
      , List.map (type_validate h) (Access.signatures h)
        @ List.map (type_check h) (Access.definitions h) )
end