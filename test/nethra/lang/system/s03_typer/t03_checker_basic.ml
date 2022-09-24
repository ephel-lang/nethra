open Nethra.Lang.Ast.Term.Construct
open Nethra.Lang.Ast.Proof
open Nethra.Lang.Ast.Hypothesis.Construct
open Nethra.Lang.Ast.Hypothesis.Access
open Nethra.Lang.System.Type

module Theory = struct
  let type_in_type = true
end

module rec TypeChecker : Specs.Checker =
  Checker.Impl (Theory) (Infer.Impl (Theory) (TypeChecker))

let check_type0_type1 () =
  let hypothesis = create
  and term = kind 0
  and term' = kind 1 in
  let proof = TypeChecker.(hypothesis |- term <= term') in
  Alcotest.(check bool) "type0" true (is_success proof)

let check_type0_type2 () =
  let hypothesis = create
  and term = kind 0
  and term' = kind 2 in
  let proof = TypeChecker.(hypothesis |- term <= term') in
  Alcotest.(check bool) "type0" true (is_success proof)

let check_int_value () =
  let hypothesis = create
  and term = int 42
  and term' = id "int" in
  let proof = TypeChecker.(hypothesis |- term <= term') in
  Alcotest.(check bool) "int" true (is_success proof)

let check_not_int_value () =
  let hypothesis = create
  and term = int 42
  and term' = id "string" in
  let proof = TypeChecker.(hypothesis |- term <= term') in
  Alcotest.(check bool) "int" false (is_success proof)

let check_char_value () =
  let hypothesis = create
  and term = char '4'
  and term' = id "char" in
  let proof = TypeChecker.(hypothesis |- term <= term') in
  Alcotest.(check bool) "char" true (is_success proof)

let check_string_value () =
  let hypothesis = create
  and term = string "4"
  and term' = id "string" in
  let proof = TypeChecker.(hypothesis |- term <= term') in
  Alcotest.(check bool) "string" true (is_success proof)

let check_id () =
  let hypothesis = add_signature create ("x", id "T")
  and term = id "x"
  and term' = id "T" in
  let proof = TypeChecker.(hypothesis |- term <= term') in
  Alcotest.(check bool) "string" true (is_success proof)

let cases =
  let open Alcotest in
  ( "Check basic terms"
  , [
      test_case "Γ ⊢ Type_i : Type_{i+1}" `Quick check_type0_type1
    ; test_case "Γ ⊢ Type_i : Type_{i+2}" `Quick check_type0_type2
    ; test_case "Γ ⊢ 42 : int" `Quick check_int_value
    ; test_case "Γ ⊢ 42 : string" `Quick check_not_int_value
    ; test_case "Γ ⊢ \'4\' : char" `Quick check_char_value
    ; test_case "Γ ⊢ \"4\" : string" `Quick check_string_value
    ; test_case "Γ, x:T ⊢ x : T" `Quick check_id
    ] )