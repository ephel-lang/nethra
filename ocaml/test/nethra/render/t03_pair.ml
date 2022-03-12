open Nethra.Ast.Term
open Common

let render_sigma () =
  let repr = render @@ Builders.(sigma "x" (kind 0) (id "x")) in
  Alcotest.(check string) "sigma" "Σ(x:type0).x" repr

let render_pair () =
  let repr = render @@ Builders.(pair (kind 0) (id "x")) in
  Alcotest.(check string) "sigma" "(type0,x)" repr

let render_fst () =
  let repr = render @@ Builders.(fst (id "x")) in
  Alcotest.(check string) "fst" "fst x" repr

let render_snd () =
  let repr = render @@ Builders.(snd (id "x")) in
  Alcotest.(check string) "snd" "snd x" repr

let cases =
  let open Alcotest in
  ( "Render pair terms and types"
  , [
      test_case "Σ(x:type0).x" `Quick render_sigma
    ; test_case "(type0,x)" `Quick render_pair
    ; test_case "fst x" `Quick render_fst
    ; test_case "snd x" `Quick render_snd
    ] )
