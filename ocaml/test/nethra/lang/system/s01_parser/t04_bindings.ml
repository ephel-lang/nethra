open Common
open Nethra.Syntax.Source
open Nethra.Toy.Parser

let parser_signature () =
  let open Binding.Impl (Parsec) in
  let result =
    response render_binding
    @@ binding ()
    @@ Parsec.source (Utils.chars_of_string "sig a : b -> c")
  and expected = (Some "sig a : b -> c", true) in
  Alcotest.(check (pair (option string) bool)) "signature" expected result

let parser_definition () =
  let open Binding.Impl (Parsec) in
  let result =
    response render_binding
    @@ binding ()
    @@ Parsec.source (Utils.chars_of_string "def a = (b).c")
  and expected = (Some "def a = (b).(c)", true) in
  Alcotest.(check (pair (option string) bool)) "definition" expected result

let cases =
  let open Alcotest in
  ( "Binding Parser"
  , [
      test_case "signature" `Quick parser_signature
    ; test_case "definition" `Quick parser_definition
    ] )
