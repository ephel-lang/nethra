open Nethra.Ast.Term.Builders
open Nethra.Ast.Proof
open Nethra.Ast.Bindings.Builders
open Nethra.Ast.Bindings.Access
open Nethra.System
module rec TypeChecker : Specs.Checker = Checker.Impl (Infer.Impl (TypeChecker))

let check_sum () =
  let bindings = add_signatures create [ ("int", kind 0); ("char", kind 0) ]
  and term = sum (id "int") (id "char")
  and term' = kind 0 in
  let proof = TypeChecker.(bindings |- term <?:> term') in
  Alcotest.(check bool) "sum" true (is_success proof)

let check_inl () =
  let bindings = add_signatures create [ ("int", kind 0); ("char", kind 0) ]
  and term = inl (int 1)
  and term' = sum (id "int") (id "char") in
  let proof = TypeChecker.(bindings |- term <?:> term') in
  Alcotest.(check bool) "sum inl" true (is_success proof)

let check_inl_wrong () =
  let bindings = add_signatures create [ ("int", kind 0); ("char", kind 0) ]
  and term = inl (char '1')
  and term' = sum (id "int") (id "char") in
  let proof = TypeChecker.(bindings |- term <?:> term') in
  Alcotest.(check bool) "sum inl wrong" false (is_success proof)

let check_inr () =
  let bindings = add_signatures create [ ("int", kind 0); ("char", kind 0) ]
  and term = inr (char '1')
  and term' = sum (id "int") (id "char") in
  let proof = TypeChecker.(bindings |- term <?:> term') in
  Alcotest.(check bool) "sum inr" true (is_success proof)

let cases =
  let open Alcotest in
  ( "Check sum terms"
  , [
      test_case "Γ ⊢ int + char : Type_0" `Quick check_sum
    ; test_case "Γ ⊢ inl 1 : int + char" `Quick check_inl
    ; test_case "Γ ⊢ inl '1' : int + char" `Quick check_inl_wrong
    ; test_case "Γ ⊢ inr '1' : int + char" `Quick check_inr
    ] )
