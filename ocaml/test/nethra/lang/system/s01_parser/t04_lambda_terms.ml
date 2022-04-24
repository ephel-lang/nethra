open Common
open Nethra.Syntax.Source
open Nethra.Toy.Parser

let parser_lambda_implicit () =
  let open Expression.Impl (Parsec) in
  let result =
    response render @@ term () @@ Parsec.source (Utils.chars_of_string "{a}.(c)")
  and expected = (Some "{a}.c", true) in
  Alcotest.(check (pair (option string) bool)) "lambda implicit" expected result

let parser_lambda_explicit () =
  let open Expression.Impl (Parsec) in
  let result =
    response render @@ term () @@ Parsec.source (Utils.chars_of_string "(a).(c)")
  and expected = (Some "(a).c", true) in
  Alcotest.(check (pair (option string) bool)) "lambda explicit" expected result

let cases =
  let open Alcotest in
  ( "Lambda Parser"
  , [
      test_case "lambda implicit" `Quick parser_lambda_implicit
    ; test_case "lambda explicit" `Quick parser_lambda_explicit
    ] )
