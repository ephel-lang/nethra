open Nethra.Ast.Term
open Common

let render_sum () =
  let repr = render @@ Builders.(sum (kind 0) (id "x")) in
  Alcotest.(check string) "sum" "type0 | x" repr

let render_inl () =
  let repr = render @@ Builders.(inl (id "x")) in
  Alcotest.(check string) "inl" "inl x" repr

let render_inr () =
  let repr = render @@ Builders.(inr (id "x")) in
  Alcotest.(check string) "inr" "inr x" repr

let render_case () =
  let repr = render @@ Builders.(case (id "x") (id "y") (id "z")) in
  Alcotest.(check string) "case" "case x y z" repr

let cases =
  let open Alcotest in
  ( "Render sum types"
  , [
      test_case "type0 | x" `Quick render_sum
    ; test_case "inl x" `Quick render_inl
    ; test_case "inr x" `Quick render_inr
    ; test_case "case x y z" `Quick render_case
    ] )
