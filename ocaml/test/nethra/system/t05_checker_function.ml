open Nethra.Ast
open Nethra.System
module rec TypeChecker : Specs.Checker = Checker.Impl (Infer.Impl (TypeChecker))

let check_pi () =
  let bindings = Bindings.Builders.create
  and term = Term.Builders.(pi "x" (kind 0) (id "x"))
  and term' = Term.Builders.kind 0 in
  let proof = TypeChecker.(bindings |- term <?:> term') in
  Alcotest.(check bool) "pi" true (Proof.is_success proof)

let check_lambda () =
  let bindings = Bindings.Builders.create
  and term = Term.Builders.(lambda "y" (id "y"))
  and term' = Term.Builders.(pi "x" (id "int") (id "int")) in
  let proof = TypeChecker.(bindings |- term <?:> term') in
  Alcotest.(check bool) "lambda" true (Proof.is_success proof)

let check_lambda_dep () =
  let bindings = Bindings.Builders.create
  and term = Term.Builders.(lambda "T" (lambda "y" (id "y")))
  and term' = Term.Builders.(pi "x" (kind 0) (pi "y" (id "x") (id "x"))) in
  let proof = TypeChecker.(bindings |- term <?:> term') in
  Alcotest.(check bool) "lambda dep" true (Proof.is_success proof)

let check_lambda_implicit () =
  let bindings = Bindings.Builders.create
  and term = Term.Builders.(lambda ~implicit:true "y" (id "y"))
  and term' = Term.Builders.(pi ~implicit:true "x" (id "int") (id "int")) in
  let proof = TypeChecker.(bindings |- term <?:> term') in
  Alcotest.(check bool) "lambda" true (Proof.is_success proof)

let check_lambda_not_implicit () =
  let bindings = Bindings.Builders.create
  and term = Term.Builders.(lambda ~implicit:true "y" (id "y"))
  and term' = Term.Builders.(pi "x" (id "int") (id "int")) in
  let proof = TypeChecker.(bindings |- term <?:> term') in
  Alcotest.(check bool) "lambda not implicit" false (Proof.is_success proof)

let check_lambda_implicit_tactic () =
  let bindings = Bindings.Builders.create
  and term = Term.Builders.(lambda "y" (id "y"))
  and term' =
    Term.Builders.(
      pi ~implicit:true "x" (kind 0) (pi "y" (id "int") (id "int")))
  in
  let proof = TypeChecker.(bindings |- term <?:> term') in
  let () = Nethra.Render.Proof.render Format.std_formatter proof in
  Alcotest.(check bool) "lambda implicit tactic" true (Proof.is_success proof)

let cases =
  let open Alcotest in
  ( "Check function terms"
  , [
      test_case "Γ ⊢ Π(x:M).N : Type_0" `Quick check_pi
    ; test_case "Γ ⊢ λ(y).y : Π(x:int).int" `Quick check_lambda
    ; test_case "Γ ⊢ λ(X).λ(y).y : Π(x:Type_0).Π(y:x).x" `Quick check_lambda_dep
    ; test_case "Γ ⊢ λ{y}.y : Π{x:int}.int" `Quick check_lambda_implicit
    ; test_case "Γ ⊢ λ{y}.y : Π(x:int).int" `Quick check_lambda_not_implicit
    ; test_case "Γ ⊢ λ{y}.y : Π(x:int).int" `Quick check_lambda_implicit
    ; test_case "Γ ⊢ λ(y).y : Π{X:Type_0}.Π(x:int).int" `Quick
        check_lambda_implicit_tactic
    ] )
