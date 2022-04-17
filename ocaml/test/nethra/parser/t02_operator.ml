open Nethra.Source
open Nethra.Parsec

let response r =
  let open Response.Destruct in
  fold
    ~success:(fun (a, b, _) -> (Some a, b))
    ~failure:(fun (b, _) -> (None, b))
    r

module Parsec = Parsers.Parsec (Sources.FromChars)

let parser_seq () =
  let open Parsers.Monad (Parsec) in
  let open Parsers.Operator (Parsec) in
  let result = response @@ (return 'a' <~> return 1) @@ Parsec.source []
  and expected = (Some ('a', 1), false) in
  Alcotest.(check (pair (option (pair char int)) bool))
    "sequence" expected result

let parser_seq_left () =
  let open Parsers.Monad (Parsec) in
  let open Parsers.Operator (Parsec) in
  let result = response @@ (return 'a' <~< return 1) @@ Parsec.source []
  and expected = (Some 'a', false) in
  Alcotest.(check (pair (option char) bool)) "sequence left" expected result

let parser_seq_right () =
  let open Parsers.Monad (Parsec) in
  let open Parsers.Operator (Parsec) in
  let result = response @@ (return 'a' >~> return 1) @@ Parsec.source []
  and expected = (Some 1, false) in
  Alcotest.(check (pair (option int) bool)) "sequence right" expected result

let parser_choice_left () =
  let open Parsers.Monad (Parsec) in
  let open Parsers.Operator (Parsec) in
  let result = response @@ (return 'a' <|> return 'b') @@ Parsec.source []
  and expected = (Some 'a', false) in
  Alcotest.(check (pair (option char) bool)) "choice left" expected result

let parser_choice_right () =
  let open Parsers.Monad (Parsec) in
  let open Parsers.Eval (Parsec) in
  let open Parsers.Operator (Parsec) in
  let result = response @@ (fail <|> return 'b') @@ Parsec.source []
  and expected = (Some 'b', false) in
  Alcotest.(check (pair (option char) bool)) "choice right" expected result

let parser_choice_fail () =
  let open Parsers.Monad (Parsec) in
  let open Parsers.Eval (Parsec) in
  let open Parsers.Operator (Parsec) in
  let result =
    response @@ (fail ~consumed:true <|> return 'b') @@ Parsec.source []
  and expected = (None, true) in
  Alcotest.(check (pair (option char) bool)) "choice fail" expected result

let parser_satisfy () =
  let open Parsers.Monad (Parsec) in
  let open Parsers.Eval (Parsec) in
  let open Parsers.Operator (Parsec) in
  let result = response @@ (return 1 <?> ( = ) 1) @@ Parsec.source []
  and expected = (Some 1, false) in
  Alcotest.(check (pair (option int) bool)) "satisfy" expected result

let cases =
  let open Alcotest in
  ( "Operator Parser"
  , [
      test_case "sequence" `Quick parser_seq
    ; test_case "sequence left" `Quick parser_seq_left
    ; test_case "sequence right" `Quick parser_seq_right
    ; test_case "choice left" `Quick parser_choice_left
    ; test_case "choice right" `Quick parser_choice_right
    ; test_case "choice fail" `Quick parser_choice_fail
    ; test_case "satisfy" `Quick parser_satisfy
    ] )
