open Common
open Nethra.Syntax.Parser

let parser_opt_some () =
  let open Parsers.Atomic (Parsec) in
  let open Parsers.Occurrence (Parsec) in
  let result = response @@ opt any @@ Parsec.source [ 'a' ]
  and expected = (Some (Some 'a'), true) in
  Alcotest.(check (pair (option (option char)) bool)) "opt some" expected result

let parser_opt_none () =
  let open Parsers.Atomic (Parsec) in
  let open Parsers.Occurrence (Parsec) in
  let result = response @@ opt any @@ Parsec.source []
  and expected = (Some None, false) in
  Alcotest.(check (pair (option (option char)) bool)) "opt none" expected result

let parser_rep_one () =
  let open Parsers.Atomic (Parsec) in
  let open Parsers.Occurrence (Parsec) in
  let result = response @@ rep any @@ Parsec.source [ 'a' ]
  and expected = (Some [ 'a' ], true) in
  Alcotest.(check (pair (option (list char)) bool)) "rep two" expected result

let parser_rep_two () =
  let open Parsers.Atomic (Parsec) in
  let open Parsers.Occurrence (Parsec) in
  let result = response @@ rep any @@ Parsec.source [ 'a'; 'b' ]
  and expected = (Some [ 'a'; 'b' ], true) in
  Alcotest.(check (pair (option (list char)) bool)) "rep two" expected result

let parser_opt_rep_none () =
  let open Parsers.Atomic (Parsec) in
  let open Parsers.Occurrence (Parsec) in
  let result = response @@ opt_rep any @@ Parsec.source []
  and expected = (Some [], false) in
  Alcotest.(check (pair (option (list char)) bool))
    "opt rep none" expected result

let parser_opt_rep_one () =
  let open Parsers.Atomic (Parsec) in
  let open Parsers.Occurrence (Parsec) in
  let result = response @@ opt_rep any @@ Parsec.source [ 'a' ]
  and expected = (Some [ 'a' ], true) in
  Alcotest.(check (pair (option (list char)) bool))
    "opt rep one" expected result

let parser_opt_rep_two () =
  let open Parsers.Atomic (Parsec) in
  let open Parsers.Occurrence (Parsec) in
  let result = response @@ opt_rep any @@ Parsec.source [ 'a'; 'b' ]
  and expected = (Some [ 'a'; 'b' ], true) in
  Alcotest.(check (pair (option (list char)) bool))
    "opt rep two" expected result

let cases =
  let open Alcotest in
  ( "Occurrence Parser"
  , [
      test_case "opt some" `Quick parser_opt_some
    ; test_case "opt none" `Quick parser_opt_none
    ; test_case "rep one" `Quick parser_rep_one
    ; test_case "rep two" `Quick parser_rep_two
    ; test_case "opt rep empty" `Quick parser_opt_rep_none
    ; test_case "opt rep one" `Quick parser_opt_rep_one
    ; test_case "opt rep two" `Quick parser_opt_rep_two
    ] )
