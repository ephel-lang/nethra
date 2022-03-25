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

let check_hole () =
  let hypothesis = add_signature create ("x", kind 0)
  and term = hole "x"
  and term' = kind 0 in
  let proof = TypeChecker.(hypothesis |- term <?:> term') in
  Alcotest.(check bool) "hole" true (is_success proof)

let check_hole_set () =
  let hypothesis = add_signatures create [ ("x", kind 0); ("int", kind 0) ]
  and term = hole ~r:(ref (Some (id "int"))) "x"
  and term' = kind 0 in
  let proof = TypeChecker.(hypothesis |- term <?:> term') in
  Alcotest.(check bool) "hole set" true (is_success proof)

let check_hole_set_wrong () =
  let hypothesis = add_signatures create [ ("x", kind 0); ("int", kind 0) ]
  and term = hole ~r:(ref (Some (int 1))) "x"
  and term' = kind 0 in
  let proof = TypeChecker.(hypothesis |- term <?:> term') in
  Alcotest.(check bool) "hole set wrong" false (is_success proof)

let cases =
  let open Alcotest in
  ( "Check hole terms"
  , [
      test_case "Γ ⊢ ?x : Type_O" `Quick check_hole
    ; test_case "Γ ⊢ ?x=int : Type_O" `Quick check_hole_set
    ; test_case "Γ ⊢ ?x=1 : Type_O" `Quick check_hole_set_wrong
    ] )
