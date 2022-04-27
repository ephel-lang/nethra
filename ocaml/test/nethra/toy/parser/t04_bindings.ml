open Common
open Nethra.Syntax.Source
open Nethra.Syntax.Parser
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

let parser_program () =
  let open Parsers.Occurrence (Parsec) in
  let open Parsers.Operator (Parsec) in
  let open Basic.Impl (Parsec) in
  let open Binding.Impl (Parsec) in
  let program =
    {toy|
      sig bool : type
      sig true : type
      sig false : type

      sig True : true
      sig False : false

      def bool = true | false
  |toy}
  in
  let result =
    response render_bindings
    @@ (skip >~> opt_rep (binding ()))
    @@ Parsec.source (Utils.chars_of_string program)
  and expected =
    ( Some
        "sig bool : type0 sig true : type0 sig false : type0 sig True : true \
         sig False : false def bool = (true) | (false)"
    , true )
  in
  Alcotest.(check (pair (option string) bool)) "program" expected result

let cases =
  let open Alcotest in
  ( "Binding Parser"
  , [
      test_case "signature" `Quick parser_signature
    ; test_case "definition" `Quick parser_definition
    ; test_case "program" `Quick parser_program
    ] )
