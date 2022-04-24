open Common
open Nethra.Syntax.Source
open Nethra.Toy.Parser

let parser_pi_implicit () =
  let open Expression.Impl (Parsec) in
  let result =
    response render
    @@ term ()
    @@ Parsec.source (Utils.chars_of_string "{a:b} -> c")
  and expected = (Some "{a:b} -> c", true) in
  Alcotest.(check (pair (option string) bool)) "pi implicit" expected result

let parser_pi_explicit () =
  let open Expression.Impl (Parsec) in
  let result =
    response render
    @@ term ()
    @@ Parsec.source (Utils.chars_of_string "(a:b) -> c")
  and expected = (Some "(a:b) -> c", true) in
  Alcotest.(check (pair (option string) bool)) "pi explicit" expected result

let parser_sigma () =
  let open Expression.Impl (Parsec) in
  let result =
    response render
    @@ term ()
    @@ Parsec.source (Utils.chars_of_string "(a:b) * c")
  and expected = (Some "(a:b) * c", true) in
  Alcotest.(check (pair (option string) bool)) "sigma" expected result

let cases =
  let open Alcotest in
  ( "PI and Sigma Terms Parser"
  , [
      test_case "pi implicit" `Quick parser_pi_implicit
    ; test_case "pi explicit" `Quick parser_pi_explicit
    ; test_case "sigma" `Quick parser_sigma
    ] )
