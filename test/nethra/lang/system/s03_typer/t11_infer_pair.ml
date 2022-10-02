open Common
open Preface.Option.Functor
open Nethra.Lang.Ast.Term.Construct
open Nethra.Lang.Ast.Proof
open Nethra.Lang.Ast.Proof.Destruct
open Nethra.Lang.Ast.Hypothesis.Construct
open Nethra.Lang.Ast.Hypothesis.Access
open Nethra.Lang.System.Type

module Theory = struct
  let type_in_type = true
end

module rec TypeInfer : Specs.Infer =
  Infer.Impl (Theory) (Checker.Impl (Theory) (TypeInfer))

let infer_sigma () =
  let hypothesis = create
  and term = sigma "x" (kind 0) (id "x")
  and expect = kind 0 in
  let proof = TypeInfer.(hypothesis |- term => ()) in
  let term' = get_type proof in
  Alcotest.(check (pair (option string) bool))
    "sigma"
    (Some (render expect), true)
    (term' <&> render, is_success proof)

let infer_pair () =
  let hypothesis = add_signature create ("char", kind 0)
  and term = pair (id "char") (char 'c')
  and expect = sigma "_" (kind 0) (id "char") in
  let proof = TypeInfer.(hypothesis |- term => ()) in
  let term' = get_type proof in
  Alcotest.(check (pair (option string) bool))
    "pair"
    (Some (render expect), true)
    (term' <&> render, is_success proof)

let infer_pair_fst () =
  let hypothesis = add_signature create ("p", sigma "n" (kind 0) (id "n"))
  and term = fst (id "p")
  and expect = kind 0 in
  let proof = TypeInfer.(hypothesis |- term => ()) in
  let term' = get_type proof in
  Alcotest.(check (pair (option string) bool))
    "pair fst"
    (Some (render expect), true)
    (term' <&> render, is_success proof)

let infer_pair_snd () =
  let hypothesis = add_signature create ("p", sigma "n" (kind 0) (id "n"))
  and term = snd (id "p")
  and expect = fst (id "p") in
  let proof = TypeInfer.(hypothesis |- term => ()) in
  let term' = get_type proof in
  Alcotest.(check (pair (option string) bool))
    "pair snd"
    (Some (render expect), true)
    (term' <&> render, is_success proof)

let cases =
  let open Alcotest in
  ( "Infer pair terms"
  , [
      test_case "Γ ⊢ (x:Type0) * x : Type_0" `Quick infer_sigma
    ; test_case "Γ ⊢ (char, 'a') : (x:Type0) * char" `Quick infer_pair
    ; test_case "Γ ⊢ fst p : Type_0 " `Quick infer_pair_fst
    ; test_case "Γ ⊢ snd p : fst p " `Quick infer_pair_snd
    ] )