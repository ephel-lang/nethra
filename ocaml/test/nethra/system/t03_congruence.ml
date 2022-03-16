open Nethra.Ast.Term.Builders
open Nethra.Ast.Bindings.Builders
open Nethra.Ast.Proof
open Nethra.System
open Congruence

let congruence_type0 () =
  let bindings = create
  and term = kind 0
  and term' = kind 0 in
  let proof = bindings |- term =?= term' in
  Alcotest.(check bool) "type0" true (is_success proof)

let congruence_int () =
  let bindings = create
  and term = int 0
  and term' = int 0 in
  let proof = bindings |- term =?= term' in
  Alcotest.(check bool) "int" true (is_success proof)

let congruence_char () =
  let bindings = create
  and term = char '0'
  and term' = char '0' in
  let proof = bindings |- term =?= term' in
  Alcotest.(check bool) "char" true (is_success proof)

let congruence_string () =
  let bindings = create
  and term = string "0"
  and term' = string "0" in
  let proof = bindings |- term =?= term' in
  Alcotest.(check bool) "string" true (is_success proof)

let congruence_id () =
  let bindings = create
  and term = id "x"
  and term' = id "x" in
  let proof = bindings |- term =?= term' in
  Alcotest.(check bool) "id" true (is_success proof)

let congruence_id_diff () =
  let bindings = create
  and term = id "x"
  and term' = id "y" in
  let proof = bindings |- term =?= term' in
  Alcotest.(check bool) "id" false (is_success proof)

let congruence_pi () =
  let bindings = create
  and term = pi "x" (kind 0) (id "x")
  and term' = pi "y" (kind 0) (id "y") in
  let proof = bindings |- term =?= term' in
  Alcotest.(check bool) "pi" true (is_success proof)

let congruence_pi_implicit () =
  let bindings = create
  and term = pi ~implicit:true "x" (kind 0) (id "x")
  and term' = pi ~implicit:true "y" (kind 0) (id "y") in
  let proof = bindings |- term =?= term' in
  Alcotest.(check bool) "pi implicit" true (is_success proof)

let congruence_pi_implicit_diff () =
  let bindings = create
  and term = pi ~implicit:true "x" (kind 0) (id "x")
  and term' = pi "y" (kind 0) (id "y") in
  let proof = bindings |- term =?= term' in
  Alcotest.(check bool) "pi implicit diff" false (is_success proof)

let congruence_lambda () =
  let bindings = create
  and term = lambda "x" (id "x")
  and term' = lambda "y" (id "y") in
  let proof = bindings |- term =?= term' in
  Alcotest.(check bool) "lambda" true (is_success proof)

let congruence_lambda_implicit () =
  let bindings = create
  and term = lambda ~implicit:true "x" (id "x")
  and term' = lambda ~implicit:true "y" (id "y") in
  let proof = bindings |- term =?= term' in
  Alcotest.(check bool) "lambda implicit" true (is_success proof)

let congruence_lambda_implicit_diff () =
  let bindings = create
  and term = lambda ~implicit:true "x" (id "x")
  and term' = lambda "y" (id "y") in
  let proof = bindings |- term =?= term' in
  Alcotest.(check bool) "lambda implicit diff" false (is_success proof)

let congruence_apply () =
  let bindings = create
  and term = apply (id "x") (id "y")
  and term' = apply (id "x") (id "y") in
  let proof = bindings |- term =?= term' in
  Alcotest.(check bool) "apply" true (is_success proof)

let congruence_apply_implicit () =
  let bindings = create
  and term = apply ~implicit:true (id "x") (id "y")
  and term' = apply ~implicit:true (id "x") (id "y") in
  let proof = bindings |- term =?= term' in
  Alcotest.(check bool) "apply" true (is_success proof)

let congruence_apply_with_reduce () =
  let bindings = create
  and term = apply (lambda "x" (apply (id "x") (id "y"))) (id "x")
  and term' = apply (id "x") (id "y") in
  let proof = bindings |- term =?= term' in
  let () = Nethra.Render.Proof.render Format.std_formatter proof in
  Alcotest.(check bool) "apply with reduce" true (is_success proof)

let congruence_sigma () =
  let bindings = create
  and term = sigma "x" (kind 0) (id "x")
  and term' = sigma "y" (kind 0) (id "y") in
  let proof = bindings |- term =?= term' in
  Alcotest.(check bool) "sigma" true (is_success proof)

let congruence_pair () =
  let bindings = create
  and term = pair (kind 0) (id "x")
  and term' = pair (kind 0) (id "x") in
  let proof = bindings |- term =?= term' in
  Alcotest.(check bool) "pair" true (is_success proof)

let congruence_fst () =
  let bindings = create
  and term = fst (id "x")
  and term' = fst (id "x") in
  let proof = bindings |- term =?= term' in
  Alcotest.(check bool) "fst" true (is_success proof)

let congruence_fst_with_reduce () =
  let bindings = create
  and term = fst (pair (fst (id "x")) (id "y"))
  and term' = fst (id "x") in
  let proof = bindings |- term =?= term' in
  Alcotest.(check bool) "fst with reduce" true (is_success proof)

let congruence_snd () =
  let bindings = create
  and term = snd (id "x")
  and term' = snd (id "x") in
  let proof = bindings |- term =?= term' in
  Alcotest.(check bool) "snd" true (is_success proof)

let congruence_snd_with_reduce () =
  let bindings = create
  and term = snd (pair (id "y") (snd (id "x")))
  and term' = snd (id "x") in
  let proof = bindings |- term =?= term' in
  Alcotest.(check bool) "snd with reduce" true (is_success proof)

let congruence_sum () =
  let bindings = create
  and term = sum (id "x") (id "y")
  and term' = sum (id "x") (id "y") in
  let proof = bindings |- term =?= term' in
  Alcotest.(check bool) "sum" true (is_success proof)

let congruence_inl () =
  let bindings = create
  and term = inl (id "x")
  and term' = inl (id "x") in
  let proof = bindings |- term =?= term' in
  Alcotest.(check bool) "inl" true (is_success proof)

let congruence_inr () =
  let bindings = create
  and term = inr (id "x")
  and term' = inr (id "x") in
  let proof = bindings |- term =?= term' in
  Alcotest.(check bool) "inr" true (is_success proof)

let congruence_case () =
  let bindings = create
  and term = case (id "x") (id "y") (id "z")
  and term' = case (id "x") (id "y") (id "z") in
  let proof = bindings |- term =?= term' in
  Alcotest.(check bool) "case" true (is_success proof)

let cases =
  let open Alcotest in
  ( "Check congruence"
  , [
      test_case "Γ ⊢ Type_i ≅ Type_i" `Quick congruence_type0
    ; test_case "Γ ⊢ 1 ≅ 1" `Quick congruence_int
    ; test_case "Γ ⊢ '1' ≅ '1'" `Quick congruence_char
    ; test_case "Γ ⊢ \"1\" ≅ \"1\"" `Quick congruence_string
    ; test_case "Γ ⊢ x ≅ x" `Quick congruence_id
    ; test_case "Γ ⊢ x ≅ y <fail>" `Quick congruence_id_diff
    ; test_case "Γ ⊢ Π(x:Type_0).x ≅ Π(y:Type_0).y" `Quick congruence_pi
    ; test_case "Γ ⊢ Π{x:Type_0}.x ≅ Π{y:Type_0}.y" `Quick congruence_pi_implicit
    ; test_case "Γ ⊢ Π{x:Type_0}.x ≅ Π(y:Type_0).y <fail>" `Quick congruence_pi_implicit_diff
    ; test_case "Γ ⊢ λ(x).x ≅ λ(y).y" `Quick congruence_lambda
    ; test_case "Γ ⊢ λ{x}.x ≅ λ{y}.y" `Quick congruence_lambda_implicit
    ; test_case "Γ ⊢ x y ≅ x y" `Quick congruence_apply
    ; test_case "Γ ⊢ x {y} ≅ x {y}" `Quick congruence_apply_implicit
    ; test_case "Γ ⊢ λ(x).(x y) x ≅ x y" `Quick congruence_apply_with_reduce
    ; test_case "Γ ⊢ Σ(x:Type_0).x ≅ Σ(y:Type_0).y" `Quick congruence_sigma
    ; test_case "Γ ⊢ (Type_0, x) ≅ (Type_0, x)" `Quick congruence_pair
    ; test_case "Γ ⊢ fst x ≅ fst x" `Quick congruence_fst
    ; test_case "Γ ⊢ fst (fst x, y) ≅ fst x" `Quick congruence_fst_with_reduce
    ; test_case "Γ ⊢ snd x ≅ snd x" `Quick congruence_snd
    ; test_case "Γ ⊢ snd (y, snd x) ≅ snd x" `Quick congruence_snd_with_reduce
    ; test_case "Γ ⊢ x | y ≅ x | y" `Quick congruence_sum
    ; test_case "Γ ⊢ inl x ≅ inl x" `Quick congruence_inl
    ; test_case "Γ ⊢ inr x ≅ inr x" `Quick congruence_inr
    ; test_case "Γ ⊢ case x y z ≅ case x y z" `Quick congruence_case
    ] )
