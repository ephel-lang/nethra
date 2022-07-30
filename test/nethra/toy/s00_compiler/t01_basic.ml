open Common
open Nethra.Toy.Compiler

open Preface_stdlib.Result.Functor (struct
  type t = Nethra.Syntax.Source.Region.t Pass.error
end)

let compile_int () =
  let result =
    Pass.run
      {toy|
        -- Preamble
        sig int : type
        -----------
        sig one : int
        def one = 1
        -----------
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string))
    "basic int" expected (string_of_error result)

let compile_string () =
  let result =
    Pass.run
      {toy|
        -- Preamble
        sig string : type
        -----------
        sig one : string
        def one = "1"
        -----------
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string))
    "basic string" expected (string_of_error result)

let compile_char () =
  let result =
    Pass.run
      {toy|
        -- Preamble
        sig char : type
        -----------
        sig one : char
        def one = '1'
        -----------
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string))
    "basic char" expected (string_of_error result)

let cases =
  let open Alcotest in
  ( "Basic Compiler"
  , [
      test_case "basic int" `Quick compile_int
    ; test_case "basic string" `Quick compile_string
    ; test_case "basic char" `Quick compile_char
    ] )
