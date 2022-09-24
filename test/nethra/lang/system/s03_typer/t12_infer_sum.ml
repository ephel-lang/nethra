open Common
open Preface.Option.Functor
open Nethra.Lang.Ast.Term.Construct
open Nethra.Lang.Ast.Proof
open Nethra.Lang.Ast.Hypothesis.Construct
open Nethra.Lang.Ast.Hypothesis.Access
open Nethra.Lang.System.Type

module Theory = struct
  let type_in_type = true
end

module rec TypeInfer : Specs.Infer =
  Infer.Impl (Theory) (Checker.Impl (Theory) (TypeInfer))

let infer_sum () =
  let hypothesis = add_signatures create [ ("int", kind 0); ("char", kind 0) ]
  and term = sum (id "int") (id "char")
  and expect = kind 0 in
  let term', proof = TypeInfer.(hypothesis |- term => ()) in
  Alcotest.(check (pair (option string) bool))
    "sum"
    (Some (render expect), true)
    (term' <&> render, is_success proof)

let infer_inl () =
  let hypothesis = add_signatures create [ ("int", kind 0); ("char", kind 0) ]
  and term = inl (int 1)
  and expect = sum (id "int") (hole "_0") in
  let term', proof = TypeInfer.(hypothesis |- term => ()) in
  Alcotest.(check (pair (option string) bool))
    "sum inl"
    (Some (render expect), true)
    (term' <&> render, is_success proof)

let infer_inr () =
  let hypothesis = add_signatures create [ ("int", kind 0); ("char", kind 0) ]
  and term = inr (char '1')
  and expect = sum (hole "_0") (id "char") in
  let term', proof = TypeInfer.(hypothesis |- term => ()) in
  Alcotest.(check (pair (option string) bool))
    "sum inr"
    (Some (render expect), true)
    (term' <&> render, is_success proof)

let cases =
  let open Alcotest in
  ( "Infer sum terms"
  , [
      test_case "Γ ⊢ int + char : Type_0" `Quick infer_sum
    ; test_case "Γ ⊢ inl 1 : int + ?" `Quick infer_inl
    ; test_case "Γ ⊢ inr '1' : ? + char" `Quick infer_inr
    ] )