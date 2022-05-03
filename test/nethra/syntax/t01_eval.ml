open Common
open Nethra.Syntax.Parser

let parser_eos () =
  let open Parsers.Eval (Parsec) in
  let result = response @@ eos @@ Parsec.source []
  and expected = (Some (), false) in
  Alcotest.(check (pair (option unit) bool)) "eos" expected result

let parser_eos_fail () =
  let open Parsers.Eval (Parsec) in
  let result = response @@ eos @@ Parsec.source [ 'a' ]
  and expected = (None, false) in
  Alcotest.(check (pair (option unit) bool)) "eos fail" expected result

let parser_return () =
  let open Parsers.Eval (Parsec) in
  let result = response @@ return 'a' @@ Parsec.source []
  and expected = (Some 'a', false) in
  Alcotest.(check (pair (option char) bool)) "return" expected result

let parser_fail () =
  let open Parsers.Eval (Parsec) in
  let result = response @@ fail @@ Parsec.source []
  and expected = (None, false) in
  Alcotest.(check (pair (option char) bool)) "fail" expected result

let parser_fail_consumed () =
  let open Parsers.Eval (Parsec) in
  let result = response @@ fail ~consumed:true @@ Parsec.source []
  and expected = (None, true) in
  Alcotest.(check (pair (option char) bool)) "fail consumed" expected result

let parser_do_lazy () =
  let open Parsers.Eval (Parsec) in
  let result = response @@ do_lazy (fun () -> return 'a') @@ Parsec.source []
  and expected = (Some 'a', false) in
  Alcotest.(check (pair (option char) bool)) "do_lazy" expected result

let parser_do_try () =
  let open Parsers.Eval (Parsec) in
  let result = response @@ do_try (fail ~consumed:true) @@ Parsec.source []
  and expected = (None, false) in
  Alcotest.(check (pair (option char) bool)) "do_try" expected result

let parser_lookahead () =
  let open Parsers.Eval (Parsec) in
  let result = response @@ do_try (fail ~consumed:true) @@ Parsec.source []
  and expected = (None, false) in
  Alcotest.(check (pair (option char) bool)) "lookahead" expected result

let parser_satisfy_true () =
  let open Parsers.Eval (Parsec) in
  let result = response @@ satisfy (return 1) (( = ) 1) @@ Parsec.source []
  and expected = (Some 1, false) in
  Alcotest.(check (pair (option int) bool)) "satisfy" expected result

let parser_satisfy_false () =
  let open Parsers.Eval (Parsec) in
  let result = response @@ satisfy (return 1) (( = ) 2) @@ Parsec.source []
  and expected = (None, false) in
  Alcotest.(check (pair (option int) bool)) "satisfy false" expected result

let cases =
  let open Alcotest in
  ( "Basic Parser"
  , [
      test_case "eos" `Quick parser_eos
    ; test_case "eos fail" `Quick parser_eos_fail
    ; test_case "return" `Quick parser_return
    ; test_case "fail" `Quick parser_fail
    ; test_case "fail consumed" `Quick parser_fail_consumed
    ; test_case "do_lazy" `Quick parser_do_lazy
    ; test_case "do_try" `Quick parser_do_try
    ; test_case "lookahead" `Quick parser_lookahead
    ; test_case "satisfy" `Quick parser_satisfy_true
    ; test_case "satisfy false" `Quick parser_satisfy_false
    ] )
