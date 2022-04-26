open Common
open Nethra.Syntax.Source
open Nethra.Toy.Parser

let parser_type0 () =
  let open Expression.Impl (Parsec) in
  let result =
    response render @@ term () @@ Parsec.source (Utils.chars_of_string "type")
  and expected = (Some "type0", true) in
  Alcotest.(check (pair (option string) bool)) "type1" expected result

let parser_type1 () =
  let open Expression.Impl (Parsec) in
  let result =
    response render @@ term () @@ Parsec.source (Utils.chars_of_string "type1")
  and expected = (Some "type1", true) in
  Alcotest.(check (pair (option string) bool)) "type1" expected result

let parser_variable () =
  let open Expression.Impl (Parsec) in
  let result =
    response render
    @@ term ()
    @@ Parsec.source (Utils.chars_of_string "varName")
  and expected = (Some "varName", true) in
  Alcotest.(check (pair (option string) bool)) "variable" expected result

let parser_int () =
  let open Expression.Impl (Parsec) in
  let result =
    response render @@ term () @@ Parsec.source (Utils.chars_of_string "42")
  and expected = (Some "42", true) in
  Alcotest.(check (pair (option string) bool)) "int" expected result

let parser_string () =
  let open Expression.Impl (Parsec) in
  let result =
    response render
    @@ term ()
    @@ Parsec.source (Utils.chars_of_string "\"Hello\"")
  and expected = (Some "\"Hello\"", true) in
  Alcotest.(check (pair (option string) bool)) "string" expected result

let parser_char () =
  let open Expression.Impl (Parsec) in
  let result =
    response render @@ term () @@ Parsec.source (Utils.chars_of_string "'H'")
  and expected = (Some "'H'", true) in
  Alcotest.(check (pair (option string) bool)) "char" expected result

let cases =
  let open Alcotest in
  ( "Simple Terms Parser"
  , [
      test_case "type0" `Quick parser_type0
    ; test_case "type1" `Quick parser_type1
    ; test_case "variable" `Quick parser_variable
    ; test_case "int" `Quick parser_int
    ; test_case "string" `Quick parser_string
    ; test_case "char" `Quick parser_char
    ] )
