open Nethra.Ast
open Nethra.System
module TypeChecker = Checker.Checker (Infer.Infer)

let check_type0_type1 () =
  let bindings = Bindings.Builders.create
  and term = Term.Builders.kind 1
  and term' = Term.Builders.kind 2 in
  let proof = TypeChecker.(bindings |- (term <?:> term')) in
  Alcotest.(check bool) "type0" (Proof.is_success proof) true

let check_int_value () =
  let bindings = Bindings.Builders.create
  and term = Term.Builders.int 42
  and term' = Term.Builders.id "int" in
  let proof = TypeChecker.(bindings |- (term <?:> term')) in
  Alcotest.(check bool) "int" (Proof.is_success proof) true

let check_not_int_value () =
  let bindings = Bindings.Builders.create
  and term = Term.Builders.int 42
  and term' = Term.Builders.id "string" in
  let proof = TypeChecker.(bindings |- (term <?:> term')) in
  Alcotest.(check bool) "int" (Proof.is_success proof) false

let check_char_value () =
  let bindings = Bindings.Builders.create
  and term = Term.Builders.char '4'
  and term' = Term.Builders.id "char" in
  let proof = TypeChecker.(bindings |- (term <?:> term')) in
  Alcotest.(check bool) "char" (Proof.is_success proof) true

let check_string_value () =
  let bindings = Bindings.Builders.create
  and term = Term.Builders.string "4"
  and term' = Term.Builders.id "string" in
  let proof = TypeChecker.(bindings |- (term <?:> term')) in
  Alcotest.(check bool) "string" (Proof.is_success proof) true

let check_id () =
  let bindings =
    Bindings.Access.add_signature Bindings.Builders.create
      ("x", Term.Builders.id "T")
  and term = Term.Builders.id "x"
  and term' = Term.Builders.id "T" in
  let proof = TypeChecker.(bindings |- (term <?:> term')) in
  Alcotest.(check bool) "string" (Proof.is_success proof) true

let cases =
  let open Alcotest in
  ( "Check basic terms"
  , [
      test_case "Γ ⊢ Type_i : Type_{i+1}" `Quick check_type0_type1
    ; test_case "Γ ⊢ 42 : int" `Quick check_int_value
    ; test_case "Γ ⊢ 42 : string" `Quick check_not_int_value
    ; test_case "Γ ⊢ \'4\' : char" `Quick check_char_value
    ; test_case "Γ ⊢ \"4\" : string" `Quick check_string_value
    ; test_case "Γ, x:T ⊢ x : T" `Quick check_id
    ] )
