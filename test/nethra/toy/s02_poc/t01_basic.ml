open Expr
open Vm
open Compiler

let compile_01 () =
  let result = optimise @@ compile (Int 1)
  and expected = [ PUSH (INT 1) ] in
  Alcotest.(check string) "compile 1" (to_string expected) (to_string result)

let compile_02 () =
  let result = optimise @@ compile (Abs ("x", Var "x"))
  and expected = [ LAMBDA [] ] in
  Alcotest.(check string)
    "compile fun x -> x" (to_string expected) (to_string result)

let compile_03 () =
  let result = optimise @@ compile (Abs ("x", Unit))
  and expected = [ LAMBDA [ DROP 1; PUSH UNIT ] ] in
  Alcotest.(check string)
    "compile fun x -> unit" (to_string expected) (to_string result)

let compile_04 () =
  let result = optimise @@ compile (App (Abs ("x", Var "x"), Int 1))
  and expected = [ PUSH (INT 1) ] in
  Alcotest.(check string)
    "compile (fun x -> x) 1" (to_string expected) (to_string result)

let compile_05 () =
  let result = optimise @@ compile (App (Abs ("x", Unit), Int 1))
  and expected = [ PUSH UNIT ] in
  Alcotest.(check string)
    "compile (fun x -> unit) 1" (to_string expected) (to_string result)

let compile_06 () =
  let result =
    optimise @@ compile (App (App (Abs ("x", Abs ("y", Var "y")), Int 1), Int 2))
  and expected = [ PUSH (INT 2) ] in
  Alcotest.(check string)
    "compile (fun x y -> y) 1 2" (to_string expected) (to_string result)

let compile_07 () =
  let result =
    optimise @@ compile (App (App (Abs ("x", Abs ("y", Var "x")), Int 1), Int 2))
  and expected = [ PUSH (INT 1) ] in
  Alcotest.(check string)
    "compile (fun x y -> x) 1 2" (to_string expected) (to_string result)

let cases =
  let open Alcotest in
  ( "Basic Compilation"
  , [
      test_case "compile O1" `Quick compile_01
    ; test_case "compile O2" `Quick compile_02
    ; test_case "compile O3" `Quick compile_03
    ; test_case "compile O4" `Quick compile_04
    ; test_case "compile O5" `Quick compile_05
    ; test_case "compile O6" `Quick compile_06
    ; test_case "compile O7" `Quick compile_07
    ] )