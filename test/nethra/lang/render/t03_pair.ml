open Nethra.Lang.Ast.Term
open Common

let render_sigma () =
  let repr = render @@ Construct.(sigma "x" (kind 0) (id "x")) in
  Alcotest.(check string) "sigma" "(x:type) * x" repr

let render_pair () =
  let repr = render @@ Construct.(pair (kind 0) (id "x")) in
  Alcotest.(check string) "sigma" "(type,x)" repr

let render_fst () =
  let repr = render @@ Construct.(fst (id "x")) in
  Alcotest.(check string) "fst" "fst x" repr

let render_snd () =
  let repr = render @@ Construct.(snd (id "x")) in
  Alcotest.(check string) "snd" "snd x" repr

let cases =
  let open Alcotest in
  ( "Render pair terms and types"
  , [
      test_case "(x:type0) * x" `Quick render_sigma
    ; test_case "(type0,x)" `Quick render_pair
    ; test_case "fst x" `Quick render_fst
    ; test_case "snd x" `Quick render_snd
    ] )