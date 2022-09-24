open Common
open Nethra.Lang.Ast.Hypothesis.Construct
open Nethra.Lang.Ast.Hypothesis.Access
open Nethra.Lang.Ast.Term.Construct
open Nethra.Lang.Basic.Reduction

let reduce_type0 () =
  let term = kind 1 in
  let term' = reduce create term in
  Alcotest.(check string) "type0" (render term) (render term')

let reduce_int () =
  let term = int 1 in
  let term' = reduce create term in
  Alcotest.(check string) "int" (render term) (render term')

let reduce_char () =
  let term = char '1' in
  let term' = reduce create term in
  Alcotest.(check string) "char" (render term) (render term')

let reduce_string () =
  let term = string "1" in
  let term' = reduce create term in
  Alcotest.(check string) "string" (render term) (render term')

let reduce_id () =
  let term = id "x" in
  let term' = reduce create term in
  Alcotest.(check string) "id" (render term) (render term')

let reduce_id_bind () =
  let term = id "x" in
  let term' = reduce (add_definition create ("x", id "y")) term in
  Alcotest.(check string) "id" (render @@ id "y") (render term')

let reduce_pi () =
  let term = pi "x" (id "y") (id "y") in
  let term' = reduce create term in
  Alcotest.(check string) "pi" (render term) (render term')

let reduce_lambda () =
  let term = lambda "x" (id "y") in
  let term' = reduce create term in
  Alcotest.(check string) "lambda" (render term) (render term')

let reduce_apply () =
  let term = apply (lambda "x" (id "x")) (id "y") in
  let term' = reduce create term in
  Alcotest.(check string) "apply" (render (id "y")) (render term')

let reduce_apply_implicit () =
  let term = apply (lambda ~implicit:true "X" (lambda "x" (id "x"))) (id "y") in
  let term' = reduce create term in
  Alcotest.(check string) "apply" (render (id "y")) (render term')

let reduce_apply_not_implicit () =
  let term = apply ~implicit:true (lambda "x" (id "x")) (id "y") in
  let term' = reduce create term in
  Alcotest.(check string) "apply" (render term) (render term')

let reduce_sigma () =
  let term = sigma "x" (id "y") (id "y") in
  let term' = reduce create term in
  Alcotest.(check string) "sigma" (render term) (render term')

let reduce_fst () =
  let term = fst (pair (id "x") (id "y")) in
  let term' = reduce create term in
  Alcotest.(check string) "fst" (render (id "x")) (render term')

let reduce_snd () =
  let term = snd (pair (id "x") (id "y")) in
  let term' = reduce create term in
  Alcotest.(check string) "snd" (render (id "y")) (render term')

let reduce_sum () =
  let term = sum (id "x") (id "y") in
  let term' = reduce create term in
  Alcotest.(check string) "sum" (render term) (render term')

let reduce_inl () =
  let term = inl (id "x") in
  let term' = reduce create term in
  Alcotest.(check string) "inl" (render term) (render term')

let reduce_inr () =
  let term = inr (id "x") in
  let term' = reduce create term in
  Alcotest.(check string) "inr" (render term) (render term')

let reduce_case_left () =
  let term = case (inl (id "x")) (lambda "y" (id "y")) (id "z") in
  let term' = reduce create term in
  Alcotest.(check string) "case term" (render @@ id "x") (render term')

let reduce_case_right () =
  let term = case (inr (id "x")) (id "z") (lambda "y" (id "y")) in
  let term' = reduce create term in
  Alcotest.(check string) "case term" (render @@ id "x") (render term')

let reduce_mu () =
  let term = mu "x" (id "y") in
  let term' = reduce create term in
  Alcotest.(check string) "mu all" (render term) (render term')

let reduce_fold () =
  let term = fold (id "x") in
  let term' = reduce create term in
  Alcotest.(check string) "fold" (render term) (render term')

let reduce_unfold () =
  let term = unfold (id "x") in
  let term' = reduce create term in
  Alcotest.(check string) "unfold" (render term) (render term')

let cases =
  let open Alcotest in
  ( "Check reduction"
  , [
      test_case "type0" `Quick reduce_type0
    ; test_case "int" `Quick reduce_int
    ; test_case "char" `Quick reduce_char
    ; test_case "string" `Quick reduce_string
    ; test_case "id" `Quick reduce_id
    ; test_case "id" `Quick reduce_id_bind
    ; test_case "pi" `Quick reduce_pi
    ; test_case "lambda" `Quick reduce_lambda
    ; test_case "apply" `Quick reduce_apply
    ; test_case "apply implicit" `Quick reduce_apply_implicit
    ; test_case "apply not implicit" `Quick reduce_apply_not_implicit
    ; test_case "sigma" `Quick reduce_sigma
    ; test_case "fst" `Quick reduce_fst
    ; test_case "snd" `Quick reduce_snd
    ; test_case "sum" `Quick reduce_sum
    ; test_case "inl" `Quick reduce_inl
    ; test_case "inr" `Quick reduce_inr
    ; test_case "case left" `Quick reduce_case_left
    ; test_case "case right" `Quick reduce_case_right
    ; test_case "mu" `Quick reduce_mu
    ; test_case "fold" `Quick reduce_fold
    ; test_case "unfold" `Quick reduce_unfold
    ] )