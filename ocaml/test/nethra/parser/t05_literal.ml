open Nethra.Source
open Nethra.Parsec

let response r =
  let open Response.Destruct in
  fold
    ~success:(fun (a, b, _) -> (Some a, b))
    ~failure:(fun (b, _) -> (None, b))
    r

module Parsec = Parsers.Parsec (Sources.FromChars)

let parser_in_range () =
  let open Parsers.Literal (Parsec) in
  let result = response @@ in_range ('a', 'c') (Parsec.source [ 'b' ])
  and expected = (Some 'b', true) in
  Alcotest.(check (pair (option char) bool)) "in range" expected result

let parser_in_range_lower () =
  let open Parsers.Literal (Parsec) in
  let result = response @@ in_range ('a', 'c') (Parsec.source [ 'a' ])
  and expected = (Some 'a', true) in
  Alcotest.(check (pair (option char) bool)) "in range lower" expected result

let parser_in_range_upper () =
  let open Parsers.Literal (Parsec) in
  let result = response @@ in_range ('a', 'c') (Parsec.source [ 'c' ])
  and expected = (Some 'c', true) in
  Alcotest.(check (pair (option char) bool)) "in range upper" expected result

let parser_in_range_fail () =
  let open Parsers.Literal (Parsec) in
  let result = response @@ in_range ('a', 'c') (Parsec.source [ 'd' ])
  and expected = (None, false) in
  Alcotest.(check (pair (option char) bool)) "in range fail" expected result

let parser_digit () =
  let open Parsers.Literal (Parsec) in
  let result = response @@ digit (Parsec.source [ '0' ])
  and expected = (Some '0', true) in
  Alcotest.(check (pair (option char) bool)) "digit" expected result

let parser_alpha () =
  let open Parsers.Literal (Parsec) in
  let result = response @@ alpha (Parsec.source [ 'g' ])
  and expected = (Some 'g', true) in
  Alcotest.(check (pair (option char) bool)) "alpha" expected result

let parser_natural () =
  let open Parsers.Literal (Parsec) in
  let result =
    response @@ natural (Parsec.source (Utils.chars_of_string "1234g"))
  and expected = (Some 1234, true) in
  Alcotest.(check (pair (option int) bool)) "natural" expected result

let parser_integer () =
  let open Parsers.Literal (Parsec) in
  let result =
    response @@ integer (Parsec.source (Utils.chars_of_string "1234g"))
  and expected = (Some 1234, true) in
  Alcotest.(check (pair (option int) bool)) "integer" expected result

let parser_negative_integer () =
  let open Parsers.Literal (Parsec) in
  let result =
    response @@ integer (Parsec.source (Utils.chars_of_string "-1234g"))
  and expected = (Some (-1234), true) in
  Alcotest.(check (pair (option int) bool)) "negative integer" expected result

let parser_positive_integer () =
  let open Parsers.Literal (Parsec) in
  let result =
    response @@ integer (Parsec.source (Utils.chars_of_string "+1234g"))
  and expected = (Some 1234, true) in
  Alcotest.(check (pair (option int) bool)) "positive integer" expected result

let cases =
  let open Alcotest in
  ( "Chars Parser"
  , [
      test_case "in range" `Quick parser_in_range
    ; test_case "in range lower" `Quick parser_in_range_lower
    ; test_case "in range upper" `Quick parser_in_range_upper
    ; test_case "in range fail" `Quick parser_in_range_fail
    ; test_case "digit" `Quick parser_digit
    ; test_case "alpha" `Quick parser_alpha
    ; test_case "natural" `Quick parser_natural
    ; test_case "integer" `Quick parser_integer
    ; test_case "negative integer" `Quick parser_negative_integer
    ; test_case "positive integer" `Quick parser_positive_integer
    ] )
