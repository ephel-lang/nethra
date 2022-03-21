open Nethra.Ast.Term.Builders
open Nethra.Ast.Proof
open Nethra.Ast.Bindings.Builders
open Nethra.Ast.Bindings.Access
open Nethra.System
module rec TypeChecker : Specs.Checker = Checker.Impl (Infer.Impl (TypeChecker))

let check_hole () =
  let bindings = add_signature create ("x", kind 0)
  and term = hole "x"
  and term' = kind 0 in
  let proof = TypeChecker.(bindings |- term <?:> term') in
  Alcotest.(check bool) "hole" true (is_success proof)

let check_hole_set () =
  let bindings = add_signatures create [ ("x", kind 0); ("int", kind 0) ]
  and term = hole ~r:(ref (Some (id "int"))) "x"
  and term' = kind 0 in
  let proof = TypeChecker.(bindings |- term <?:> term') in
  Alcotest.(check bool) "hole set" true (is_success proof)

let check_hole_set_wrong () =
  let bindings = add_signatures create [ ("x", kind 0); ("int", kind 0) ]
  and term = hole ~r:(ref (Some (int 1))) "x"
  and term' = kind 0 in
  let proof = TypeChecker.(bindings |- term <?:> term') in
  Alcotest.(check bool) "hole set wrong" false (is_success proof)

let cases =
  let open Alcotest in
  ( "Check hole terms"
  , [
      test_case "Γ ⊢ ?x : Type_O" `Quick check_hole
    ; test_case "Γ ⊢ ?x=int : Type_O" `Quick check_hole_set
    ; test_case "Γ ⊢ ?x=1 : Type_O" `Quick check_hole_set_wrong
    ] )
