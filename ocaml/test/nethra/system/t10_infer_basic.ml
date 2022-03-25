open Common
open Preface.Option.Functor
open Nethra.Ast.Term.Builders
open Nethra.Ast.Proof
open Nethra.Ast.Hypothesis.Builders
open Nethra.Ast.Hypothesis.Access
open Nethra.System

module Theory = struct
  let type_in_type = true
end

module rec TypeInfer : Specs.Infer =
  Infer.Impl (Checker.Impl (Theory) (TypeInfer))

let infer_type0 () =
  let hypothesis = create
  and term = kind 0
  and expect = kind 1 in
  let term', proof = TypeInfer.(hypothesis |- term <:?> ()) in
  Alcotest.(check (pair (option string) bool))
    "pi"
    (Some (render expect), true)
    (term' <&> render, is_success proof)

let infer_int () =
  let hypothesis = create
  and term = int 0
  and expect = id "int" in
  let term', proof = TypeInfer.(hypothesis |- term <:?> ()) in
  Alcotest.(check (pair (option string) bool))
    "pi"
    (Some (render expect), true)
    (term' <&> render, is_success proof)

let infer_char () =
  let hypothesis = create
  and term = char '0'
  and expect = id "char" in

  let term', proof = TypeInfer.(hypothesis |- term <:?> ()) in
  Alcotest.(check (pair (option string) bool))
    "pi"
    (Some (render expect), true)
    (term' <&> render, is_success proof)

let infer_string () =
  let hypothesis = create
  and term = string "0"
  and expect = id "string" in
  let term', proof = TypeInfer.(hypothesis |- term <:?> ()) in
  Alcotest.(check (pair (option string) bool))
    "pi"
    (Some (render expect), true)
    (term' <&> render, is_success proof)

let infer_id () =
  let hypothesis = add_signature create ("t", kind 0)
  and term = id "t"
  and expect = kind 0 in
  let term', proof = TypeInfer.(hypothesis |- term <:?> ()) in
  Alcotest.(check (pair (option string) bool))
    "pi"
    (Some (render expect), true)
    (term' <&> render, is_success proof)

let cases =
  let open Alcotest in
  ( "Infer basic terms"
  , [
      test_case "Γ ⊢ Type_0 : Type_1" `Quick infer_type0
    ; test_case "Γ ⊢ 1 : int" `Quick infer_int
    ; test_case "Γ ⊢ '1' : char" `Quick infer_char
    ; test_case "Γ ⊢ \"1\" : string" `Quick infer_string
    ; test_case "Γ, t : Type_0 ⊢ t : Type_0" `Quick infer_id
    ] )
