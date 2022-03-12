open Nethra.Ast.Term
open Common

let render_pi () =
  let repr = render @@ Builders.(pi "x" (kind 0) (id "x")) in
  Alcotest.(check string) "pi" "Π(x:type0).x" repr

let render_pi_implicit () =
  let repr = render @@ Builders.(pi ~implicit:true "x" (kind 0) (id "x")) in
  Alcotest.(check string) "pi implicit" "Π{x:type0}.x" repr

let render_lambda () =
  let repr = render @@ Builders.(lambda "x" (id "x")) in
  Alcotest.(check string) "lambda" "λ(x).x" repr

let render_lambda_implicit () =
  let repr = render @@ Builders.(lambda ~implicit:true "x" (id "x")) in
  Alcotest.(check string) "lambda" "λ{x}.x" repr

let render_apply () =
  let repr = render @@ Builders.(apply (lambda "x" (id "x")) (id "x")) in
  Alcotest.(check string) "lambda" "λ(x).x (x)" repr

let render_apply_implicit () =
  let repr =
    render
    @@ Builders.(
         apply ~implicit:true (lambda ~implicit:true "x" (id "x")) (id "x"))
  in
  Alcotest.(check string) "lambda" "λ{x}.x {x}" repr

let cases =
  let open Alcotest in
  ( "Render function terms and types"
  , [
      test_case "Π(x:type0).x" `Quick render_pi
    ; test_case "Π{x:type0}.x" `Quick render_pi_implicit
    ; test_case "λ(x).x" `Quick render_lambda
    ; test_case "λ{x}.x" `Quick render_lambda_implicit
    ; test_case "λ(x).x (x)" `Quick render_apply
    ; test_case "λ{x}.x {x}" `Quick render_apply_implicit
    ] )
