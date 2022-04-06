open Nethra.Ast.Term.Construct
open Nethra.Ast.Proof
open Nethra.Ast.Hypothesis.Construct
open Nethra.Ast.Hypothesis.Access
open Nethra.System

module Theory = struct
  let type_in_type = true
end

module rec TypeChecker : Specs.Checker =
  Checker.Impl (Theory) (Infer.Impl (TypeChecker))

let check_sigma () =
  let hypothesis = create
  and term = sigma "x" (kind 0) (id "x")
  and term' = kind 0 in
  let proof = TypeChecker.(hypothesis |- term <= term') in
  Alcotest.(check bool) "sigma" true (is_success proof)

let check_pair () =
  let hypothesis = add_signature create ("char", kind 0)
  and term = pair (id "char") (char 'c')
  and term' = sigma "x" (kind 0) (id "x") in
  let proof = TypeChecker.(hypothesis |- term <= term') in
  Alcotest.(check bool) "pair" true (is_success proof)

let check_pair_wrong () =
  let hypothesis = add_signature create ("char", kind 0)
  and term = pair (id "char") (int 1)
  and term' = sigma "x" (kind 0) (id "x") in
  let proof = TypeChecker.(hypothesis |- term <= term') in
  Alcotest.(check bool) "pair wrong" false (is_success proof)

let check_pair_fst () =
  let hypothesis = add_signature create ("p", sigma "n" (kind 0) (id "n"))
  and term = fst (id "p")
  and term' = kind 0 in
  let proof = TypeChecker.(hypothesis |- term <= term') in
  Alcotest.(check bool) "pair fst" true (is_success proof)

let check_pair_snd () =
  let hypothesis = add_signature create ("p", sigma "n" (kind 0) (id "n"))
  and term = snd (id "p")
  and term' = fst (id "p") in
  let proof = TypeChecker.(hypothesis |- term <= term') in
  Alcotest.(check bool) "pair snd" true (is_success proof)

let cases =
  let open Alcotest in
  ( "Check pair terms"
  , [
      test_case "Γ ⊢ Σ(x:M).x : Type_0" `Quick check_sigma
    ; test_case "Γ ⊢ (char, 'a') : Σ(x:M).x" `Quick check_pair
    ; test_case "Γ ⊢ (char, 1) : Σ(x:M).x" `Quick check_pair_wrong
    ; test_case "Γ ⊢ fst p : Type_0 " `Quick check_pair_fst
    ; test_case "Γ ⊢ snd p : fst p " `Quick check_pair_snd
    ] )
