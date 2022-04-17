open Nethra.Source
open Nethra.Parsec

let response r =
  let open Response.Destruct in
  fold
    ~success:(fun (a, b, _) -> (Some a, b))
    ~failure:(fun (b, _) -> (None, b))
    r

module Parsec = Parsers.Parsec (Sources.FromChars)

let parser_char () =
  let open Parsers.Literal (Parsec) in
  let result = response @@ char 'a' @@ Parsec.source [ 'a' ]
  and expected = (Some 'a', true) in
  Alcotest.(check (pair (option char) bool)) "char" expected result

let parser_char_fail () =
  let open Parsers.Literal (Parsec) in
  let result = response @@ char 'a' @@ Parsec.source [ 'b' ]
  and expected = (None, false) in
  Alcotest.(check (pair (option char) bool)) "char fail" expected result

let parser_char_in_range () =
  let open Parsers.Literal (Parsec) in
  let result = response @@ char_in_range ('a', 'c') @@ Parsec.source [ 'b' ]
  and expected = (Some 'b', true) in
  Alcotest.(check (pair (option char) bool)) "char in range" expected result

let parser_char_in_range_lower () =
  let open Parsers.Literal (Parsec) in
  let result = response @@ char_in_range ('a', 'c') @@ Parsec.source [ 'a' ]
  and expected = (Some 'a', true) in
  Alcotest.(check (pair (option char) bool))
    "char in range lower" expected result

let parser_char_in_range_upper () =
  let open Parsers.Literal (Parsec) in
  let result = response @@ char_in_range ('a', 'c') @@ Parsec.source [ 'c' ]
  and expected = (Some 'c', true) in
  Alcotest.(check (pair (option char) bool))
    "char in range upper" expected result

let parser_char_in_range_fail () =
  let open Parsers.Literal (Parsec) in
  let result = response @@ char_in_range ('a', 'c') @@ Parsec.source [ 'd' ]
  and expected = (None, false) in
  Alcotest.(check (pair (option char) bool))
    "char in range fail" expected result

let parser_char_in_list () =
  let open Parsers.Literal (Parsec) in
  let result = response @@ char_in_list [ 'a'; 'b' ] @@ Parsec.source [ 'b' ]
  and expected = (Some 'b', true) in
  Alcotest.(check (pair (option char) bool)) "char in list" expected result

let parser_char_in_string () =
  let open Parsers.Literal (Parsec) in
  let result = response @@ char_in_string "ab" @@ Parsec.source [ 'b' ]
  and expected = (Some 'b', true) in
  Alcotest.(check (pair (option char) bool)) "char in string" expected result

let parser_digit () =
  let open Parsers.Literal (Parsec) in
  let result = response @@ digit @@ Parsec.source [ '0' ]
  and expected = (Some '0', true) in
  Alcotest.(check (pair (option char) bool)) "digit" expected result

let parser_alpha () =
  let open Parsers.Literal (Parsec) in
  let result = response @@ alpha @@ Parsec.source [ 'g' ]
  and expected = (Some 'g', true) in
  Alcotest.(check (pair (option char) bool)) "alpha" expected result

let parser_natural () =
  let open Parsers.Literal (Parsec) in
  let result =
    response @@ natural @@ Parsec.source (Utils.chars_of_string "1234g")
  and expected = (Some 1234, true) in
  Alcotest.(check (pair (option int) bool)) "natural" expected result

let parser_integer () =
  let open Parsers.Literal (Parsec) in
  let result =
    response @@ integer @@ Parsec.source (Utils.chars_of_string "1234g")
  and expected = (Some 1234, true) in
  Alcotest.(check (pair (option int) bool)) "integer" expected result

let parser_negative_integer () =
  let open Parsers.Literal (Parsec) in
  let result =
    response @@ integer @@ Parsec.source (Utils.chars_of_string "-1234g")
  and expected = (Some (-1234), true) in
  Alcotest.(check (pair (option int) bool)) "negative integer" expected result

let parser_positive_integer () =
  let open Parsers.Literal (Parsec) in
  let result =
    response @@ integer @@ Parsec.source (Utils.chars_of_string "+1234g")
  and expected = (Some 1234, true) in
  Alcotest.(check (pair (option int) bool)) "positive integer" expected result

let parser_string () =
  let open Parsers.Literal (Parsec) in
  let result =
    response @@ string "Hello" @@ Parsec.source (Utils.chars_of_string "Hello")
  and expected = (Some "Hello", true) in
  Alcotest.(check (pair (option string) bool)) "string" expected result

let parser_string_in_list () =
  let open Parsers.Literal (Parsec) in
  let result =
    response
    @@ string_in_list [ "World"; "Hello" ]
    @@ Parsec.source (Utils.chars_of_string "Hello")
  and expected = (Some "Hello", true) in
  Alcotest.(check (pair (option string) bool)) "string list" expected result

let parser_sequence () =
  let open Parsers.Operator (Parsec) in
  let open Parsers.Literal (Parsec) in
  let result =
    response
    @@ sequence (alpha <|> digit)
    @@ Parsec.source (Utils.chars_of_string "Hello123")
  and expected = (Some "Hello123", true) in
  Alcotest.(check (pair (option string) bool)) "sequence" expected result

let parser_delimited_string () =
  let open Parsers.Literal (Parsec) in
  let result =
    response
    @@ Delimited.string
    @@ Parsec.source (Utils.chars_of_string "\"Hello\"")
  and expected = (Some "Hello", true) in
  Alcotest.(check (pair (option string) bool))
    "delimited string" expected result

let parser_delimited_string_escaped () =
  let open Parsers.Literal (Parsec) in
  let result =
    response
    @@ Delimited.string
    @@ Parsec.source (Utils.chars_of_string "\"Hel\\\"lo\"")
  and expected = (Some "Hel\"lo", true) in
  Alcotest.(check (pair (option string) bool))
    "delimited string escaped" expected result

let parser_delimited_string_meta () =
  let open Parsers.Literal (Parsec) in
  let result =
    response
    @@ Delimited.string
    @@ Parsec.source (Utils.chars_of_string "\"Hel\nlo\"")
  and expected = (Some "Hel\nlo", true) in
  Alcotest.(check (pair (option string) bool))
    "delimited string meta" expected result

let parser_delimited_char () =
  let open Parsers.Literal (Parsec) in
  let result =
    response @@ Delimited.char @@ Parsec.source (Utils.chars_of_string "'H'")
  and expected = (Some 'H', true) in
  Alcotest.(check (pair (option char) bool)) "delimited char" expected result

let parser_delimited_char_escaped () =
  let open Parsers.Literal (Parsec) in
  let result =
    response @@ Delimited.char @@ Parsec.source (Utils.chars_of_string "'\\''")
  and expected = (Some '\'', true) in
  Alcotest.(check (pair (option char) bool))
    "delimited char escaped" expected result

let parser_delimited_char_meta () =
  let open Parsers.Literal (Parsec) in
  let result =
    response @@ Delimited.char @@ Parsec.source (Utils.chars_of_string "'\n'")
  and expected = (Some '\n', true) in
  Alcotest.(check (pair (option char) bool))
    "delimited char meta" expected result

let cases =
  let open Alcotest in
  ( "Chars Parser"
  , [
      test_case "char" `Quick parser_char
    ; test_case "char fail" `Quick parser_char_fail
    ; test_case "in range" `Quick parser_char_in_range
    ; test_case "in range lower" `Quick parser_char_in_range_lower
    ; test_case "in range upper" `Quick parser_char_in_range_upper
    ; test_case "in range fail" `Quick parser_char_in_range_fail
    ; test_case "in list" `Quick parser_char_in_list
    ; test_case "in string" `Quick parser_char_in_string
    ; test_case "digit" `Quick parser_digit
    ; test_case "alpha" `Quick parser_alpha
    ; test_case "natural" `Quick parser_natural
    ; test_case "integer" `Quick parser_integer
    ; test_case "negative integer" `Quick parser_negative_integer
    ; test_case "positive integer" `Quick parser_positive_integer
    ; test_case "string" `Quick parser_string
    ; test_case "string list" `Quick parser_string_in_list
    ; test_case "sequence" `Quick parser_sequence
    ; test_case "delimited string" `Quick parser_delimited_string
    ; test_case "delimited string escaped" `Quick
        parser_delimited_string_escaped
    ; test_case "delimited string meta" `Quick parser_delimited_string_meta
    ; test_case "delimited char" `Quick parser_delimited_char
    ; test_case "delimited char escaped" `Quick parser_delimited_char_escaped
    ; test_case "delimited char meta" `Quick parser_delimited_char_escaped
    ] )
