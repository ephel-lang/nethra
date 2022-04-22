open Nethra.Lang.Ast.Term.Construct
open Nethra.Lang.Ast.Proof
open Nethra.Lang.Ast.Context.Hypothesis.Construct
open Nethra.Lang.Ast.Context.Hypothesis.Access
open Nethra.Lang.System.Type

module Theory = struct
  let type_in_type = true
end

module rec TypeChecker : Specs.Checker =
  Checker.Impl (Theory) (Infer.Impl (TypeChecker))

let check_pi () =
  let hypothesis = create
  and term = pi "x" (kind 0) (id "x")
  and term' = kind 0 in
  let proof = TypeChecker.(hypothesis |- term <= term') in
  Alcotest.(check bool) "pi" true (is_success proof)

let check_lambda () =
  let hypothesis = create
  and term = lambda "y" (id "y")
  and term' = pi "x" (id "int") (id "int") in
  let proof = TypeChecker.(hypothesis |- term <= term') in
  Alcotest.(check bool) "lambda" true (is_success proof)

let check_lambda_dep () =
  let hypothesis = create
  and term = lambda "T" (lambda "y" (id "y"))
  and term' = pi "x" (kind 0) (pi "y" (id "x") (id "x")) in
  let proof = TypeChecker.(hypothesis |- term <= term') in
  Alcotest.(check bool) "lambda dep" true (is_success proof)

let check_lambda_implicit () =
  let hypothesis = create
  and term = lambda ~implicit:true "y" (id "y")
  and term' = pi ~implicit:true "x" (id "int") (id "int") in
  let proof = TypeChecker.(hypothesis |- term <= term') in
  Alcotest.(check bool) "lambda" true (is_success proof)

let check_lambda_not_implicit () =
  let hypothesis = create
  and term = lambda ~implicit:true "y" (id "y")
  and term' = pi "x" (id "int") (id "int") in
  let proof = TypeChecker.(hypothesis |- term <= term') in
  Alcotest.(check bool) "lambda not implicit" false (is_success proof)

let check_lambda_implicit_tactic () =
  let hypothesis = create
  and term = lambda "y" (id "y")
  and term' = pi ~implicit:true "x" (kind 0) (pi "y" (id "int") (id "int")) in
  let proof = TypeChecker.(hypothesis |- term <= term') in
  Alcotest.(check bool) "lambda implicit tactic" true (is_success proof)

let check_apply () =
  let hypothesis = add_signature create ("y", arrow (id "int") (id "int"))
  and term = apply (id "y") (int 1)
  and term' = id "int" in
  let proof = TypeChecker.(hypothesis |- term <= term') in
  Alcotest.(check bool) "apply" true (is_success proof)

let check_apply_implicit () =
  let hypothesis =
    add_signature create ("y", pi ~implicit:true "_" (id "int") (id "int"))
  and term = apply ~implicit:true (id "y") (int 1)
  and term' = id "int" in
  let proof = TypeChecker.(hypothesis |- term <= term') in
  Alcotest.(check bool) "apply" true (is_success proof)

let check_apply_implicit_tactic () =
  let hypothesis =
    add_signature create
      ("y", pi ~implicit:true "_" (kind 0) (arrow (id "int") (id "int")))
  and term = apply (id "y") (int 1)
  and term' = id "int" in
  let proof = TypeChecker.(hypothesis |- term <= term') in
  Alcotest.(check bool) "apply" true (is_success proof)

let cases =
  let open Alcotest in
  ( "Check function terms"
  , [
      test_case "Γ ⊢ Π(x:M).N : Type_0" `Quick check_pi
    ; test_case "Γ ⊢ λ(y).y : Π(x:int).int" `Quick check_lambda
    ; test_case "Γ ⊢ λ(X).λ(y).y : Π(x:Type_0).Π(y:x).x" `Quick check_lambda_dep
    ; test_case "Γ ⊢ λ{y}.y : Π{x:int}.int" `Quick check_lambda_implicit
    ; test_case "Γ ⊢ λ{y}.y : Π(x:int).int" `Quick check_lambda_not_implicit
    ; test_case "Γ ⊢ λ(y).y : Π{X:Type_0}.Π(x:int).int" `Quick
        check_lambda_implicit_tactic
    ; test_case "Γ ⊢ λ(y).y 1 : int" `Quick check_apply
    ; test_case "Γ ⊢ λ{y}.y {1} : int" `Quick check_apply_implicit
    ; test_case "Γ ⊢ λ{X}.λ(y).y 1 : int" `Quick check_apply_implicit_tactic
    ] )
