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

let subst_sigma_all () =
  let term = sigma "x" (id "y") (id "y") in
  let term' = substitute "y" (id "z") term in
  Alcotest.(check string)
    "sigma all"
    (render @@ sigma "x" (id "z") (id "z"))
    (render term')

let subst_sigma_bound_only () =
  let term = sigma "x" (id "x") (id "y") in
  let term' = substitute "x" (id "z") term in
  Alcotest.(check string)
    "sigma bound only"
    (render @@ sigma "x" (id "z") (id "y"))
    (render term')

let subst_pair_left () =
  let term = pair (id "x") (id "y") in
  let term' = substitute "x" (id "z") term in
  Alcotest.(check string)
    "pair left"
    (render @@ pair (id "z") (id "y"))
    (render term')

let subst_pair_right () =
  let term = pair (id "x") (id "y") in
  let term' = substitute "y" (id "z") term in
  Alcotest.(check string)
    "pair right"
    (render @@ pair (id "x") (id "z"))
    (render term')

let subst_fst () =
  let term = fst (id "x") in
  let term' = substitute "x" (id "z") term in
  Alcotest.(check string) "fst" (render @@ fst (id "z")) (render term')

let subst_snd () =
  let term = snd (id "x") in
  let term' = substitute "x" (id "z") term in
  Alcotest.(check string) "snd" (render @@ snd (id "z")) (render term')

let subst_sum_left () =
  let term = sum (id "x") (id "y") in
  let term' = substitute "x" (id "z") term in
  Alcotest.(check string)
    "sum left"
    (render @@ sum (id "z") (id "y"))
    (render term')

let subst_sum_right () =
  let term = sum (id "x") (id "y") in
  let term' = substitute "y" (id "z") term in
  Alcotest.(check string)
    "sum right"
    (render @@ sum (id "x") (id "z"))
    (render term')

let subst_inl () =
  let term = inl (id "x") in
  let term' = substitute "x" (id "z") term in
  Alcotest.(check string) "inl" (render @@ inl (id "z")) (render term')

let subst_inr () =
  let term = inr (id "x") in
  let term' = substitute "x" (id "z") term in
  Alcotest.(check string) "inr" (render @@ inr (id "z")) (render term')

let subst_case_term () =
  let term = case (id "x") (id "y") (id "z") in
  let term' = substitute "x" (id "w") term in
  Alcotest.(check string)
    "case term"
    (render @@ case (id "w") (id "y") (id "z"))
    (render term')

let subst_case_left () =
  let term = case (id "x") (id "y") (id "z") in
  let term' = substitute "y" (id "w") term in
  Alcotest.(check string)
    "case left"
    (render @@ case (id "x") (id "w") (id "z"))
    (render term')

let subst_case_right () =
  let term = case (id "x") (id "y") (id "z") in
  let term' = substitute "z" (id "w") term in
  Alcotest.(check string)
    "case right"
    (render @@ case (id "x") (id "y") (id "w"))
    (render term')

let subst_mu_all () =
  let term = mu "x" (id "y") in
  let term' = substitute "y" (id "z") term in
  Alcotest.(check string) "mu all" (render @@ mu "x" (id "z")) (render term')

let subst_not_mu () =
  let term = mu "x" (id "x") in
  let term' = substitute "x" (id "z") term in
  Alcotest.(check string) "not mu" (render term) (render term')

let subst_fold () =
  let term = fold (id "x") in
  let term' = substitute "x" (id "z") term in
  Alcotest.(check string) "fold" (render @@ fold (id "z")) (render term')

let subst_unfold () =
  let term = unfold (id "x") in
  let term' = substitute "x" (id "z") term in
  Alcotest.(check string) "unfold" (render @@ unfold (id "z")) (render term')

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
    ; test_case "sigma all" `Quick subst_sigma_all
    ; test_case "sigma bound only" `Quick subst_sigma_bound_only
    ; test_case "pair left" `Quick subst_pair_left
    ; test_case "pair right" `Quick subst_pair_right
    ; test_case "fst" `Quick subst_fst
    ; test_case "snd" `Quick subst_snd
    ; test_case "sum left" `Quick subst_sum_left
    ; test_case "sum right" `Quick subst_sum_right
    ; test_case "inl" `Quick subst_inl
    ; test_case "inr" `Quick subst_inr
    ; test_case "case term" `Quick subst_case_term
    ; test_case "case left" `Quick subst_case_left
    ; test_case "case right" `Quick subst_case_right
    ; test_case "mu all" `Quick subst_mu_all
    ; test_case "not mu" `Quick subst_not_mu
    ; test_case "fold" `Quick subst_fold
    ; test_case "unfold" `Quick subst_unfold
    ] )
