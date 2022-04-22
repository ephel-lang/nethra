open Common
open Preface.Option.Functor
open Nethra.Lang.Ast.Term.Construct
open Nethra.Lang.Ast.Proof
open Nethra.Lang.Ast.Context.Hypothesis.Construct
open Nethra.Lang.Ast.Context.Hypothesis.Access
open Nethra.Lang.System.Type

module Theory = struct
  let type_in_type = true
end

module rec TypeInfer : Specs.Infer =
  Infer.Impl (Checker.Impl (Theory) (TypeInfer))

let infer_pi () =
  let hypothesis = create
  and term = pi "x" (kind 0) (id "x")
  and expect = kind 0 in
  let term', proof = TypeInfer.(hypothesis |- term => ()) in
  Alcotest.(check (pair (option string) bool))
    "pi"
    (Some (render expect), true)
    (term' <&> render, is_success proof)

let infer_lambda_explicit () =
  let hypothesis = add_signature create ("plus", arrow (id "int") (id "int"))
  and term = lambda "x" (apply (id "plus") (id "x"))
  and expect = pi "x" (id "int") (id "int") in
  let term', proof = TypeInfer.(hypothesis |- term => ()) in
  Alcotest.(check (pair (option string) bool))
    "lambda explicit"
    (Some (render expect), true)
    (term' <&> render, is_success proof)

let infer_lambda_general_explicit () =
  let hypothesis = create
  and term = lambda "x" (id "x")
  and expect = pi ~implicit:true "x" (kind 0) (pi "x" (id "x") (id "x")) in
  let term', proof = TypeInfer.(hypothesis |- term => ()) in
  Alcotest.(check (pair (option string) bool))
    "lambda general explicit"
    (Some (render expect), true)
    (term' <&> render, is_success proof)

let infer_lambda_implicit () =
  let hypothesis = add_signature create ("plus", arrow (id "int") (id "int"))
  and term = lambda ~implicit:true "x" (apply (id "plus") (id "x"))
  and expect = pi ~implicit:true "x" (id "int") (id "int") in
  let term', proof = TypeInfer.(hypothesis |- term => ()) in
  Alcotest.(check (pair (option string) bool))
    "lambda implicit"
    (Some (render expect), true)
    (term' <&> render, is_success proof)

let infer_lambda_general_implicit () =
  let hypothesis = create
  and term = lambda ~implicit:true "x" (id "x")
  and expect =
    pi ~implicit:true "x" (kind 0) (pi ~implicit:true "x" (id "x") (id "x"))
  in
  let term', proof = TypeInfer.(hypothesis |- term => ()) in
  Alcotest.(check (pair (option string) bool))
    "lambda general explicit"
    (Some (render expect), true)
    (term' <&> render, is_success proof)

let infer_apply_explicit () =
  let hypothesis = add_signature create ("plus", arrow (id "int") (id "int"))
  and term = apply (id "plus") (int 1)
  and expect = id "int" in
  let term', proof = TypeInfer.(hypothesis |- term => ()) in
  Alcotest.(check (pair (option string) bool))
    "apply explicit"
    (Some (render expect), true)
    (term' <&> render, is_success proof)

let infer_apply_implicit () =
  let hypothesis =
    add_signature create ("plus", pi ~implicit:true "_" (id "int") (id "int"))
  and term = apply ~implicit:true (id "plus") (int 1)
  and expect = id "int" in
  let term', proof = TypeInfer.(hypothesis |- term => ()) in
  Alcotest.(check (pair (option string) bool))
    "apply implicit"
    (Some (render expect), true)
    (term' <&> render, is_success proof)

let cases =
  let open Alcotest in
  ( "Infer function terms"
  , [
      test_case "Γ ⊢ Π(x:M).N : Type_0" `Quick infer_pi
    ; test_case "Γ ⊢ λ(x).(plus x) : Π(x:int).int" `Quick infer_lambda_explicit
    ; test_case "Γ ⊢ λ(x).x : Π{x:Type_0}.Π(x:int).int" `Quick
        infer_lambda_general_explicit
    ; test_case "Γ ⊢ λ{x}.(plus x) : Π{x:int}.int" `Quick infer_lambda_implicit
    ; test_case "Γ ⊢ λ{x}.x : Π{x:Type_0}.Π{x:int}.int" `Quick
        infer_lambda_general_implicit
    ; test_case "Γ ⊢ plus 1 : int" `Quick infer_apply_explicit
    ; test_case "Γ ⊢ plus {1} : int" `Quick infer_apply_implicit
    ] )
