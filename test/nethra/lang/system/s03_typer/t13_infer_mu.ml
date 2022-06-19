open Common
open Preface.Option.Functor
open Nethra.Lang.Ast.Context.Hypothesis.Construct
open Nethra.Lang.Ast.Context.Hypothesis.Access
open Nethra.Lang.Ast.Proof
open Nethra.Lang.Ast.Term.Construct
open Nethra.Lang.System.Type

module Theory = struct
  let type_in_type = true
end

module rec TypeInfer : Specs.Infer =
  Infer.Impl (Theory) (Checker.Impl (Theory) (TypeInfer))

let infer_mu () =
  let hypothesis = add_signature create ("int", kind 0)
  and term = mu "x" (kind 0) (pi "_" (id "x") (id "int"))
  and expect = kind 0 in
  let term', proof = TypeInfer.(hypothesis |- term => ()) in
  Alcotest.(check (pair (option string) bool))
    "mu"
    (Some (render expect), true)
    (term' <&> render, is_success proof)

let infer_mu_fold () =
  let hypothesis =
    add_signature create
      ("a", pi "_" (mu "x" (kind 0) (pi "_" (id "x") (id "int"))) (id "int"))
  and term = fold (id "a") in
  let term', proof = TypeInfer.(hypothesis |- term => ()) in
  Alcotest.(check (pair (option string) bool))
    "sum fold" (None, false)
    (term' <&> render, is_success proof)

let infer_mu_unfold () =
  let hypothesis =
    add_signature create ("a", mu "x" (kind 0) (pi "_" (id "x") (id "int")))
  and term = unfold (id "a")
  and expect =
    pi "_" (mu "x" (kind 0) (pi "_" (id "x") (id "int"))) (id "int")
  in
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
