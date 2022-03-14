open Common
open Nethra.Ast.Term.Builders
open Nethra.System.Substitution.Impl

let subst_type0 () =
  let term = kind 1 in
  let term' = substitute "x" (id "y") term in
  Alcotest.(check string) "type0" (render term) (render term')

let subst_int () =
  let term = int 1 in
  let term' = substitute "x" (id "y") term in
  Alcotest.(check string) "int" (render term) (render term')

let subst_char () =
  let term = char '1' in
  let term' = substitute "x" (id "y") term in
  Alcotest.(check string) "char" (render term) (render term')

let subst_string () =
  let term = string "1" in
  let term' = substitute "x" (id "y") term in
  Alcotest.(check string) "string" (render term) (render term')

let subst_same_id () =
  let term = id "x" in
  let term' = substitute "x" (id "y") term in
  Alcotest.(check string) "id" (render @@ id "y") (render term')

let subst_not_same_id () =
  let term = id "y" in
  let term' = substitute "x" (id "y") term in
  Alcotest.(check string) "not same id" (render term) (render term')

let subst_pi_all () =
  let term = pi "x" (id "y") (id "y") in
  let term' = substitute "y" (id "z") term in
  Alcotest.(check string)
    "pi all"
    (render @@ pi "x" (id "z") (id "z"))
    (render term')

let subst_pi_bound_only () =
  let term = pi "x" (id "x") (id "y") in
  let term' = substitute "x" (id "z") term in
  Alcotest.(check string)
    "pi bound only"
    (render @@ pi "x" (id "z") (id "y"))
    (render term')

let subst_lambda_all () =
  let term = lambda "x" (id "y") in
  let term' = substitute "y" (id "z") term in
  Alcotest.(check string)
    "lambda all"
    (render @@ lambda "x" (id "z"))
    (render term')

let subst_not_lambda () =
  let term = lambda "x" (id "x") in
  let term' = substitute "x" (id "z") term in
  Alcotest.(check string) "not lambda" (render term) (render term')

let subst_apply_left () =
  let term = apply (id "x") (id "y") in
  let term' = substitute "x" (id "z") term in
  Alcotest.(check string)
    "apply left"
    (render @@ apply (id "z") (id "y"))
    (render term')

let subst_apply_right () =
  let term = apply (id "x") (id "y") in
  let term' = substitute "y" (id "z") term in
  Alcotest.(check string)
    "apply right"
    (render @@ apply (id "x") (id "z"))
    (render term')

let cases =
  let open Alcotest in
  ( "Check substitution"
  , [
      test_case "type0" `Quick subst_type0
    ; test_case "int" `Quick subst_int
    ; test_case "char" `Quick subst_char
    ; test_case "string" `Quick subst_string
    ; test_case "id" `Quick subst_same_id
    ; test_case "not same id" `Quick subst_not_same_id
    ; test_case "pi all" `Quick subst_pi_all
    ; test_case "pi bound only" `Quick subst_pi_bound_only
    ; test_case "lambda all" `Quick subst_lambda_all
    ; test_case "not lambda" `Quick subst_not_lambda
    ; test_case "apply left" `Quick subst_apply_left
    ; test_case "apply right" `Quick subst_apply_right
    ] )
