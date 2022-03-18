open Nethra.Ast.Term.Builders
open Nethra.Ast.Proof
open Nethra.Ast.Bindings.Builders
open Nethra.Ast.Bindings.Access
open Nethra.System
module rec TypeChecker : Specs.Checker = Checker.Impl (Infer.Impl (TypeChecker))

let check_mu () =
  let bindings = add_signature create ("int", kind 0)
  and term = mu "x" (pi "_" (id "x") (id "int"))
  and term' = kind 0 in
  let proof = TypeChecker.(bindings |- term <?:> term') in
  Alcotest.(check bool) "mu" true (is_success proof)

let check_mu_fold () =
  let bindings =
    add_signature create
      ("a", pi "_" (mu "x" (pi "_" (id "x") (id "int"))) (id "int"))
  and term = fold (id "a")
  and term' = mu "x" (pi "_" (id "x") (id "int")) in
  let proof = TypeChecker.(bindings |- term <?:> term') in
  Alcotest.(check bool) "mu fold" true (is_success proof)

let check_mu_unfold () =
  let bindings = add_signature create ("a", mu "x" (pi "_" (id "x") (id "int")))
  and term = unfold (id "a")
  and term' = pi "_" (mu "x" (pi "_" (id "x") (id "int"))) (id "int") in
  let proof = TypeChecker.(bindings |- term <?:> term') in
  Alcotest.(check bool) "mu unfold" true (is_success proof)

let cases =
  let open Alcotest in
  ( "Check recursive terms"
  , [
      test_case "Γ ⊢ μ(x).(x -> int) : Type_O" `Quick check_mu
    ; test_case "Γ ⊢ fold a : μ(x).(x -> int)" `Quick check_mu_fold
    ; test_case "Γ ⊢ unfold a : μ(x).(x -> int) -> int" `Quick check_mu_fold
    ] )
