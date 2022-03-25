open Nethra.Ast.Term.Builders
open Nethra.Ast.Proof
open Nethra.Ast.Hypothesis.Builders
open Nethra.Ast.Hypothesis.Access
open Nethra.System

module Theory = struct
  let type_in_type = true
end

module rec TypeChecker : Specs.Checker =
  Checker.Impl (Theory) (Infer.Impl (TypeChecker))

let check_sum () =
  let hypothesis = add_signatures create [ ("int", kind 0); ("char", kind 0) ]
  and term = sum (id "int") (id "char")
  and term' = kind 0 in
  let proof = TypeChecker.(hypothesis |- term <?:> term') in
  Alcotest.(check bool) "sum" true (is_success proof)

let check_inl () =
  let hypothesis = add_signatures create [ ("int", kind 0); ("char", kind 0) ]
  and term = inl (int 1)
  and term' = sum (id "int") (id "char") in
  let proof = TypeChecker.(hypothesis |- term <?:> term') in
  Alcotest.(check bool) "sum inl" true (is_success proof)

let check_inl_wrong () =
  let hypothesis = add_signatures create [ ("int", kind 0); ("char", kind 0) ]
  and term = inl (char '1')
  and term' = sum (id "int") (id "char") in
  let proof = TypeChecker.(hypothesis |- term <?:> term') in
  Alcotest.(check bool) "sum inl wrong" false (is_success proof)

let check_inr () =
  let hypothesis = add_signatures create [ ("int", kind 0); ("char", kind 0) ]
  and term = inr (char '1')
  and term' = sum (id "int") (id "char") in
  let proof = TypeChecker.(hypothesis |- term <?:> term') in
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
