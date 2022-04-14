open Common
open Preface.Option.Functor
open Nethra.Ast.Term.Construct
open Nethra.Ast.Proof
open Nethra.Ast.Hypothesis.Construct
open Nethra.Ast.Hypothesis.Access
open Nethra.System

module Theory = struct
  let type_in_type = true
end

module rec TypeInfer : Specs.Infer =
  Infer.Impl (Checker.Impl (Theory) (TypeInfer))

let infer_mu () =
  let hypothesis = add_signature create ("int", kind 0)
  and term = mu "x" (pi "_" (id "x") (id "int"))
  and expect = kind 0 in
  let term', proof = TypeInfer.(hypothesis |- term => ()) in
  Alcotest.(check (pair (option string) bool))
    "mu"
    (Some (render expect), true)
    (term' <&> render, is_success proof)

let infer_mu_fold () =
  let hypothesis =
    add_signature create
      ("a", pi "_" (mu "x" (pi "_" (id "x") (id "int"))) (id "int"))
  and term = fold (id "a") in
  let term', proof = TypeInfer.(hypothesis |- term => ()) in
  Alcotest.(check (pair (option string) bool))
    "sum fold" (None, false)
    (term' <&> render, is_success proof)

let infer_mu_unfold () =
  let hypothesis =
    add_signature create ("a", mu "x" (pi "_" (id "x") (id "int")))
  and term = unfold (id "a")
  and expect = pi "_" (mu "x" (pi "_" (id "x") (id "int"))) (id "int") in
  let term', proof = TypeInfer.(hypothesis |- term => ()) in
  Alcotest.(check (pair (option string) bool))
    "mu unfold"
    (Some (render expect), true)
    (term' <&> render, is_success proof)

let cases =
  let open Alcotest in
  ( "Infer recursive terms"
  , [
      test_case "Γ ⊢ μ(x).(x -> int) : Type_O" `Quick infer_mu
    ; test_case "Γ ⊢ fold a : μ(x).(x -> int)" `Quick infer_mu_fold
    ; test_case "Γ ⊢ unfold a : μ(x).(x -> int) -> int" `Quick infer_mu_fold
    ] )
