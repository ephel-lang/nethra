open Common
open Nethra.Syntax.Source
open Nethra.Toy.Parser

let parser_comment_line () =
  let open Basic.Impl (Parsec) in
  let result =
    response Stdlib.Fun.id
    @@ comment_line
    @@ Parsec.source (Utils.chars_of_string "-- Hello World")
  and expected = (Some " Hello World", true) in
  Alcotest.(check (pair (option string) bool)) "comment_line" expected result

let parser_comment_block () =
  let open Basic.Impl (Parsec) in
  let result =
    response Stdlib.Fun.id
    @@ comment_block
    @@ Parsec.source (Utils.chars_of_string "-{ Hello World }-")
  and expected = (Some " Hello World ", true) in
  Alcotest.(check (pair (option string) bool)) "comment_block" expected result

let parser_identifier () =
  let open Basic.Impl (Parsec) in
  let result =
    response Stdlib.Fun.id
    @@ identifier
    @@ Parsec.source (Utils.chars_of_string "Hello World")
  and expected = (Some "Hello", true) in
  Alcotest.(check (pair (option string) bool)) "identifier" expected result

let parser_identifier_in_reserved () =
  let open Basic.Impl (Parsec) in
  let result =
    response Stdlib.Fun.id
    @@ identifier
    @@ Parsec.source (Utils.chars_of_string "let")
  and expected = (None, false) in
  Alcotest.(check (pair (option string) bool))
    "identifier in reserved" expected result

let parser_reserved_identifier () =
  let open Basic.Impl (Parsec) in
  let result =
    response Stdlib.Fun.id
    @@ Reserved._CASE_
    @@ Parsec.source (Utils.chars_of_string "case")
  and expected = (Some "case", true) in
  Alcotest.(check (pair (option string) bool))
    "reserved identifier" expected result

let parser_operator () =
  let open Basic.Impl (Parsec) in
  let result =
    response Stdlib.Fun.id
    @@ operator
    @@ Parsec.source (Utils.chars_of_string ">>=")
  and expected = (Some ">>=", true) in
  Alcotest.(check (pair (option string) bool)) "operator" expected result

let parser_operator_in_reserved () =
  let open Basic.Impl (Parsec) in
  let result =
    response Stdlib.Fun.id
    @@ operator
    @@ Parsec.source (Utils.chars_of_string "->")
  and expected = (None, false) in
  Alcotest.(check (pair (option string) bool))
    "operator in reserved" expected result

let parser_reserved_operator () =
  let open Basic.Impl (Parsec) in
  let result =
    response Stdlib.Fun.id
    @@ Reserved._ARROW_
    @@ Parsec.source (Utils.chars_of_string "->")
  and expected = (Some "->", true) in
  Alcotest.(check (pair (option string) bool))
    "reserved operator" expected result

let cases =
  let open Alcotest in
  ( "Basic Parser"
  , [
      test_case "comment_line" `Quick parser_comment_line
    ; test_case "comment_block" `Quick parser_comment_block
    ; test_case "identifier" `Quick parser_identifier
    ; test_case "identifier in reserved" `Quick parser_identifier_in_reserved
    ; test_case "reserved identifier" `Quick parser_reserved_identifier
    ; test_case "operator" `Quick parser_operator
    ; test_case "operator in reserved" `Quick parser_operator_in_reserved
    ; test_case "reserved operator" `Quick parser_reserved_operator
    ] )
