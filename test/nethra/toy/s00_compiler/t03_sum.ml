open Common
open Nethra.Toy.Compiler

let compile_basic_sum () =
  let open Preface_stdlib.Result.Functor (struct
    type t = string
  end) in
  let result =
    Stage.run
      {toy|
        --- Preamble
        sig int : type
        sig char : type
        ------------
        sig ic : int | char -> type
        def ic = (x).(case x (_).char (_).int)

        sig m1 : ic (inl 1)
        def m1 = 'c'

        sig m2 : ic (inr m1)
        def m2 = 1
        ------------
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string)) "basic sum type" expected result

(* TODO(didier) *)
let compile_recursive_sum () =
  let open Preface_stdlib.Result.Functor (struct
    type t = string
  end) in
  let result =
    Stage.run
      {toy|
        --- Preamble
        sig unit : type
        sig Unit  : unit
        sig int : type
        ------------
        sig list : (type) -> type
        def list = (X).rec(l).(unit | X * l)
        sig nil  : {X:type} -> list X
        sig cons : {X:type} -> X -> list X -> list X
        ------------
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string)) "recursive sum type" expected result

let cases =
  let open Alcotest in
  ( "Sum Compiler"
  , [
      test_case "basic sum type" `Quick compile_basic_sum
    ; test_case "recursive sum type" `Quick compile_recursive_sum
    ] )
