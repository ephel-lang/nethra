open Nethra.Ast.Term.Construct
open Nethra.Ast.Hypothesis.Construct
open Nethra.Ast.Proof
open Nethra.System

open Equivalence.Impl (struct
  let type_in_type = true
end)

let equivalence_type0 () =
  let hypothesis = create
  and term = kind 0
  and term' = kind 0 in
  let proof = hypothesis |- term =?= term' in
  Alcotest.(check bool) "type0" true (is_success proof)

let equivalence_int () =
  let hypothesis = create
  and term = int 0
  and term' = int 0 in
  let proof = hypothesis |- term =?= term' in
  Alcotest.(check bool) "int" true (is_success proof)

let equivalence_char () =
  let hypothesis = create
  and term = char '0'
  and term' = char '0' in
  let proof = hypothesis |- term =?= term' in
  Alcotest.(check bool) "char" true (is_success proof)

let equivalence_string () =
  let hypothesis = create
  and term = string "0"
  and term' = string "0" in
  let proof = hypothesis |- term =?= term' in
  Alcotest.(check bool) "string" true (is_success proof)

let equivalence_id () =
  let hypothesis = create
  and term = id "x"
  and term' = id "x" in
  let proof = hypothesis |- term =?= term' in
  Alcotest.(check bool) "id" true (is_success proof)

let equivalence_id_diff () =
  let hypothesis = create
  and term = id "x"
  and term' = id "y" in
  let proof = hypothesis |- term =?= term' in
  Alcotest.(check bool) "id" false (is_success proof)

let equivalence_pi () =
  let hypothesis = create
  and term = pi "x" (kind 0) (id "x")
  and term' = pi "y" (kind 0) (id "y") in
  let proof = hypothesis |- term =?= term' in
  Alcotest.(check bool) "pi" true (is_success proof)

let equivalence_pi_implicit () =
  let hypothesis = create
  and term = pi ~implicit:true "x" (kind 0) (id "x")
  and term' = pi ~implicit:true "y" (kind 0) (id "y") in
  let proof = hypothesis |- term =?= term' in
  Alcotest.(check bool) "pi implicit" true (is_success proof)

let equivalence_pi_implicit_diff () =
  let hypothesis = create
  and term = pi ~implicit:true "x" (kind 0) (id "x")
  and term' = pi "y" (kind 0) (id "y") in
  let proof = hypothesis |- term =?= term' in
  Alcotest.(check bool) "pi implicit diff" false (is_success proof)

let equivalence_lambda () =
  let hypothesis = create
  and term = lambda "x" (id "x")
  and term' = lambda "y" (id "y") in
  let proof = hypothesis |- term =?= term' in
  Alcotest.(check bool) "lambda" true (is_success proof)

let equivalence_lambda_implicit () =
  let hypothesis = create
  and term = lambda ~implicit:true "x" (id "x")
  and term' = lambda ~implicit:true "y" (id "y") in
  let proof = hypothesis |- term =?= term' in
  Alcotest.(check bool) "lambda implicit" true (is_success proof)

let equivalence_lambda_implicit_diff () =
  let hypothesis = create
  and term = lambda ~implicit:true "x" (id "x")
  and term' = lambda "y" (id "y") in
  let proof = hypothesis |- term =?= term' in
  Alcotest.(check bool) "lambda implicit diff" false (is_success proof)

let equivalence_apply () =
  let hypothesis = create
  and term = apply (id "x") (id "y")
  and term' = apply (id "x") (id "y") in
  let proof = hypothesis |- term =?= term' in
  Alcotest.(check bool) "apply" true (is_success proof)

let equivalence_apply_implicit () =
  let hypothesis = create
  and term = apply ~implicit:true (id "x") (id "y")
  and term' = apply ~implicit:true (id "x") (id "y") in
  let proof = hypothesis |- term =?= term' in
  Alcotest.(check bool) "apply" true (is_success proof)

let equivalence_apply_with_reduce () =
  let hypothesis = create
  and term = apply (lambda "x" (apply (id "x") (id "y"))) (id "x")
  and term' = apply (id "x") (id "y") in
  let proof = hypothesis |- term =?= term' in
  let () = Nethra.Render.Proof.render Format.std_formatter proof in
  Alcotest.(check bool) "apply with reduce" true (is_success proof)

let equivalence_sigma () =
  let hypothesis = create
  and term = sigma "x" (kind 0) (id "x")
  and term' = sigma "y" (kind 0) (id "y") in
  let proof = hypothesis |- term =?= term' in
  Alcotest.(check bool) "sigma" true (is_success proof)

let equivalence_pair () =
  let hypothesis = create
  and term = pair (kind 0) (id "x")
  and term' = pair (kind 0) (id "x") in
  let proof = hypothesis |- term =?= term' in
  Alcotest.(check bool) "pair" true (is_success proof)

let equivalence_fst () =
  let hypothesis = create
  and term = fst (id "x")
  and term' = fst (id "x") in
  let proof = hypothesis |- term =?= term' in
  Alcotest.(check bool) "fst" true (is_success proof)

let equivalence_fst_with_reduce () =
  let hypothesis = create
  and term = fst (pair (fst (id "x")) (id "y"))
  and term' = fst (id "x") in
  let proof = hypothesis |- term =?= term' in
  Alcotest.(check bool) "fst with reduce" true (is_success proof)

let equivalence_snd () =
  let hypothesis = create
  and term = snd (id "x")
  and term' = snd (id "x") in
  let proof = hypothesis |- term =?= term' in
  Alcotest.(check bool) "snd" true (is_success proof)

let equivalence_snd_with_reduce () =
  let hypothesis = create
  and term = snd (pair (id "y") (snd (id "x")))
  and term' = snd (id "x") in
  let proof = hypothesis |- term =?= term' in
  Alcotest.(check bool) "snd with reduce" true (is_success proof)

let equivalence_sum () =
  let hypothesis = create
  and term = sum (id "x") (id "y")
  and term' = sum (id "x") (id "y") in
  let proof = hypothesis |- term =?= term' in
  Alcotest.(check bool) "sum" true (is_success proof)

let equivalence_inl () =
  let hypothesis = create
  and term = inl (id "x")
  and term' = inl (id "x") in
  let proof = hypothesis |- term =?= term' in
  Alcotest.(check bool) "inl" true (is_success proof)

let equivalence_inr () =
  let hypothesis = create
  and term = inr (id "x")
  and term' = inr (id "x") in
  let proof = hypothesis |- term =?= term' in
  Alcotest.(check bool) "inr" true (is_success proof)

let equivalence_case () =
  let hypothesis = create
  and term = case (id "x") (id "y") (id "z")
  and term' = case (id "x") (id "y") (id "z") in
  let proof = hypothesis |- term =?= term' in
  Alcotest.(check bool) "case" true (is_success proof)

let equivalence_case_inl () =
  let hypothesis = create
  and term = case (inl (id "x")) (id "y") (id "z")
  and term' = apply (id "y") (id "x") in
  let proof = hypothesis |- term =?= term' in
  Alcotest.(check bool) "case" true (is_success proof)

let equivalence_case_inr () =
  let hypothesis = create
  and term = case (inr (id "x")) (id "y") (id "z")
  and term' = apply (id "z") (id "x") in
  let proof = hypothesis |- term =?= term' in
  Alcotest.(check bool) "case" true (is_success proof)

let equivalence_mu () =
  let hypothesis = create
  and term = mu "x" (id "x")
  and term' = mu "y" (id "y") in
  let proof = hypothesis |- term =?= term' in
  Alcotest.(check bool) "mu" true (is_success proof)

let equivalence_fold () =
  let hypothesis = create
  and term = fold (id "x")
  and term' = fold (id "x") in
  let proof = hypothesis |- term =?= term' in
  Alcotest.(check bool) "fold" true (is_success proof)

let equivalence_unfold () =
  let hypothesis = create
  and term = unfold (id "x")
  and term' = unfold (id "x") in
  let proof = hypothesis |- term =?= term' in
  Alcotest.(check bool) "unfold" true (is_success proof)

let equivalence_hole () =
  let hypothesis = create
  and term = hole "x"
  and term' = hole "x" in
  let proof = hypothesis |- term =?= term' in
  Alcotest.(check bool) "hole same" true (is_success proof)

let equivalence_hole_left () =
  let hypothesis = create
  and term = hole "x"
  and term' = id "x" in
  let proof = hypothesis |- term =?= term' in
  Alcotest.(check bool) "hole left" true (is_success proof)

let equivalence_hole_right () =
  let hypothesis = create
  and term = id "x"
  and term' = hole "x" in
  let proof = hypothesis |- term =?= term' in
  Alcotest.(check bool) "hole right" true (is_success proof)

let equivalence_hole_left_ref () =
  let hypothesis = create
  and term = hole ~r:(ref (Some (id "x"))) "x"
  and term' = id "x" in
  let proof = hypothesis |- term =?= term' in
  Alcotest.(check bool) "hole left ref" true (is_success proof)

let equivalence_hole_right_ref () =
  let hypothesis = create
  and term = id "x"
  and term' = hole ~r:(ref (Some (id "x"))) "x" in
  let proof = hypothesis |- term =?= term' in
  Alcotest.(check bool) "hole right ref" true (is_success proof)

let cases =
  let open Alcotest in
  ( "Check equivalence"
  , [
      test_case "Γ ⊢ Type_i ≅ Type_i" `Quick equivalence_type0
    ; test_case "Γ ⊢ 1 ≅ 1" `Quick equivalence_int
    ; test_case "Γ ⊢ '1' ≅ '1'" `Quick equivalence_char
    ; test_case "Γ ⊢ \"1\" ≅ \"1\"" `Quick equivalence_string
    ; test_case "Γ ⊢ x ≅ x" `Quick equivalence_id
    ; test_case "Γ ⊢ x ≅ y <fail>" `Quick equivalence_id_diff
    ; test_case "Γ ⊢ Π(x:Type_0).x ≅ Π(y:Type_0).y" `Quick equivalence_pi
    ; test_case "Γ ⊢ Π{x:Type_0}.x ≅ Π{y:Type_0}.y" `Quick
        equivalence_pi_implicit
    ; test_case "Γ ⊢ Π{x:Type_0}.x ≅ Π(y:Type_0).y <fail>" `Quick
        equivalence_pi_implicit_diff
    ; test_case "Γ ⊢ λ(x).x ≅ λ(y).y" `Quick equivalence_lambda
    ; test_case "Γ ⊢ λ{x}.x ≅ λ{y}.y" `Quick equivalence_lambda_implicit
    ; test_case "Γ ⊢ x y ≅ x y" `Quick equivalence_apply
    ; test_case "Γ ⊢ x {y} ≅ x {y}" `Quick equivalence_apply_implicit
    ; test_case "Γ ⊢ λ(x).(x y) x ≅ x y" `Quick equivalence_apply_with_reduce
    ; test_case "Γ ⊢ Σ(x:Type_0).x ≅ Σ(y:Type_0).y" `Quick equivalence_sigma
    ; test_case "Γ ⊢ (Type_0, x) ≅ (Type_0, x)" `Quick equivalence_pair
    ; test_case "Γ ⊢ fst x ≅ fst x" `Quick equivalence_fst
    ; test_case "Γ ⊢ fst (fst x, y) ≅ fst x" `Quick equivalence_fst_with_reduce
    ; test_case "Γ ⊢ snd x ≅ snd x" `Quick equivalence_snd
    ; test_case "Γ ⊢ snd (y, snd x) ≅ snd x" `Quick equivalence_snd_with_reduce
    ; test_case "Γ ⊢ x | y ≅ x | y" `Quick equivalence_sum
    ; test_case "Γ ⊢ inl x ≅ inl x" `Quick equivalence_inl
    ; test_case "Γ ⊢ inr x ≅ inr x" `Quick equivalence_inr
    ; test_case "Γ ⊢ case x y z ≅ case x y z" `Quick equivalence_case
    ; test_case "Γ ⊢ case (inl x) y z ≅ y x" `Quick equivalence_case_inl
    ; test_case "Γ ⊢ case (inr x) y z ≅ z x" `Quick equivalence_case_inr
    ; test_case "Γ ⊢ μ(x).x ≅ μ(y).y" `Quick equivalence_mu
    ; test_case "Γ ⊢ fold x ≅ fold x" `Quick equivalence_fold
    ; test_case "Γ ⊢ unfold x ≅ unfold x" `Quick equivalence_unfold
    ; test_case "Γ ⊢ ?x ≅ ?x" `Quick equivalence_hole
    ; test_case "Γ ⊢ ?x ≅ x" `Quick equivalence_hole_left
    ; test_case "Γ ⊢ x ≅ ?x" `Quick equivalence_hole_right
    ; test_case "Γ ⊢ ?x↦x ≅ x" `Quick equivalence_hole_left_ref
    ; test_case "Γ ⊢ x ≅ ?x↦x" `Quick equivalence_hole_right_ref
    ] )
