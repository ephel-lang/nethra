open Nethra.Source
open Nethra.Parsec

let response r =
  let open Response.Destruct in
  fold
    ~success:(fun (a, b, _) -> (Some a, b))
    ~failure:(fun (b, _) -> (None, b))
    r

module Parsec = Parsers.Parsec (Sources.FromChars)

let parser_any () =
  let open Parsers.Atomic (Parsec) in
  let result = response @@ any @@ Parsec.source [ 'a' ]
  and expected = (Some 'a', true) in
  Alcotest.(check (pair (option char) bool)) "any" expected result

let parser_atom () =
  let open Parsers.Atomic (Parsec) in
  let result = response @@ atom 'a' @@ Parsec.source [ 'a' ]
  and expected = (Some 'a', true) in
  Alcotest.(check (pair (option char) bool)) "atom" expected result

let parser_atom_fail () =
  let open Parsers.Atomic (Parsec) in
  let result = response @@ atom 'a' @@ Parsec.source [ 'b' ]
  and expected = (None, false) in
  Alcotest.(check (pair (option char) bool)) "atom fail" expected result

let parser_atom_in () =
  let open Parsers.Atomic (Parsec) in
  let result = response @@ atom_in [ 'a'; 'b' ] @@ Parsec.source [ 'b' ]
  and expected = (Some 'b', true) in
  Alcotest.(check (pair (option char) bool)) "atom in" expected result

let parser_atom_in_fail () =
  let open Parsers.Atomic (Parsec) in
  let result = response @@ atom_in [ 'a'; 'b' ] @@ Parsec.source [ 'c' ]
  and expected = (None, false) in
  Alcotest.(check (pair (option char) bool)) "atom in fail" expected result

let parser_atoms () =
  let open Parsers.Atomic (Parsec) in
  let result = response @@ atoms [ 'a'; 'b' ] @@ Parsec.source [ 'a'; 'b' ]
  and expected = (Some [ 'a'; 'b' ], true) in
  Alcotest.(check (pair (option (list char)) bool)) "atoms" expected result

let parser_atoms_fail () =
  let open Parsers.Atomic (Parsec) in
  let result = response @@ atoms [ 'a'; 'b' ] @@ Parsec.source [ 'a'; 'c' ]
  and expected = (None, false) in
  Alcotest.(check (pair (option (list char)) bool)) "atoms fail" expected result

let parser_not_atom () =
  let open Parsers.Atomic (Parsec) in
  let result = response @@ (not (atom 'a')) @@ Parsec.source [ 'b' ]
  and expected = (Some 'b', true) in
  Alcotest.(check (pair (option char) bool)) "not atom" expected result

let parser_not_atom_fail () =
  let open Parsers.Atomic (Parsec) in
  let result = response @@ (not (atom 'a')) @@ Parsec.source [ 'a' ]
  and expected = (None, false) in
  Alcotest.(check (pair (option char) bool)) "not atom fail" expected result

let cases =
  let open Alcotest in
  ( "Atom Parser"
  , [
      test_case "any" `Quick parser_any
    ; test_case "atom" `Quick parser_atom
    ; test_case "atom fail" `Quick parser_atom_fail
    ; test_case "atom in" `Quick parser_atom_in
    ; test_case "atom in fail" `Quick parser_atom_in_fail
    ; test_case "atoms" `Quick parser_atoms
    ; test_case "atoms fail" `Quick parser_atoms_fail
    ; test_case "not atom" `Quick parser_not_atom
    ; test_case "not atom fail" `Quick parser_not_atom_fail
    ] )
