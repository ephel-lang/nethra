open Nethra.Lang.Ast.Term
open Common

let render_pi () =
  let repr = render @@ Construct.(pi "x" (kind 0) (id "x")) in
  Alcotest.(check string) "pi" "Π(x:type).x" repr

let render_pi_implicit () =
  let repr = render @@ Construct.(pi ~implicit:true "x" (kind 0) (id "x")) in
  Alcotest.(check string) "pi implicit" "Π{x:type}.x" repr

let render_lambda () =
  let repr = render @@ Construct.(lambda "x" (id "x")) in
  Alcotest.(check string) "lambda" "λ(x).(x)" repr

let render_lambda_implicit () =
  let repr = render @@ Construct.(lambda ~implicit:true "x" (id "x")) in
  Alcotest.(check string) "lambda" "λ{x}.(x)" repr

let render_apply () =
  let repr = render @@ Construct.(apply (lambda "x" (id "x")) (id "x")) in
  Alcotest.(check string) "lambda" "λ(x).(x) x" repr

let render_apply_implicit () =
  let repr =
    render
    @@ Construct.(
         apply ~implicit:true (lambda ~implicit:true "x" (id "x")) (id "x"))
  in
  Alcotest.(check string) "lambda" "λ{x}.(x) {x}" repr

let render_lambda_apply () =
  let repr = render @@ Construct.(lambda "x" (apply (id "x") (int 1))) in
  Alcotest.(check string) "lambda/apply" "λ(x).(x 1)" repr

let cases =
  let open Alcotest in
  ( "Render function terms and types"
  , [
      test_case "Π(x:type0).x" `Quick render_pi
    ; test_case "Π{x:type0}.x" `Quick render_pi_implicit
    ; test_case "λ(x).(x)" `Quick render_lambda
    ; test_case "λ(x).(x)" `Quick render_lambda_apply
    ; test_case "λ{x}.(x)" `Quick render_lambda_implicit
    ; test_case "λ(x).(x) x" `Quick render_apply
    ; test_case "λ{x}.(x) {x}" `Quick render_apply_implicit
    ; test_case "λ(x).(x 1)" `Quick render_lambda_apply
    ] )
