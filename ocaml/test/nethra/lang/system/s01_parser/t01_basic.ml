open Common
open Nethra.Syntax.Source
open Nethra.Lang.System.Parser

let parser_comment_line () =
  let open Basic.Impl (Sources.FromChars) in
  let result = response @@ comment_line @@ Parsec.source (Utils.chars_of_string "-- Hello World")
  and expected = (Some " Hello World", true) in
  Alcotest.(check (pair (option string) bool)) "comment_line" expected result

let parser_comment_block () =
  let open Basic.Impl (Sources.FromChars) in
  let result = response @@ comment_block @@ Parsec.source (Utils.chars_of_string "-{ Hello World }-")
  and expected = (Some " Hello World ", true) in
  Alcotest.(check (pair (option string) bool)) "comment_block" expected result

let cases =
  let open Alcotest in
  ( "Basic Parser"
  , [
      test_case "comment_line" `Quick parser_comment_line
    ; test_case "comment_block" `Quick parser_comment_block
    ]
    )