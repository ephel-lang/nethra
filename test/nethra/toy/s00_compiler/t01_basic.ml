open Common
open Nethra.Toy.Compiler

let compile_int () =
  let open Preface_stdlib.Result.Functor (struct
    type t = string
  end) in
  let result =
    Stage.run
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
  Alcotest.(check (result bool string)) "basic int" expected result

let compile_string () =
  let open Preface_stdlib.Result.Functor (struct
    type t = string
  end) in
  let result =
    Stage.run
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
  Alcotest.(check (result bool string)) "basic string" expected result

let compile_char () =
  let open Preface_stdlib.Result.Functor (struct
    type t = string
  end) in
  let result =
    Stage.run
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
  Alcotest.(check (result bool string)) "basic char" expected result

let cases =
  let open Alcotest in
  ( "Basic Compiler"
  , [
      test_case "basic int" `Quick compile_int
    ; test_case "basic string" `Quick compile_string
    ; test_case "basic char" `Quick compile_char
    ] )
