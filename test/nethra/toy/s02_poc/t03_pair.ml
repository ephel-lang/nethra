open Expr
open Vm
open Compiler

let compile_01 () =
  let result =
    compile (Abs ("f", Abs ("x", App (App (Var "f", Var "x"), Var "x"))))
  and expected = [ LAMBDA [ LAMBDA [ DIG 1; DUP 1; EXEC; DIG 1; EXEC ] ] ] in
  Alcotest.(check string)
    "compile (fun f x -> f x x)" (to_string expected) (to_string result)

let compile_02 () =
  let result =
    compile (Abs ("f", Abs ("x", App (App (Var "x", Var "f"), Var "x"))))
  and expected = [ LAMBDA [ LAMBDA [ DUP 0; DIG 2; EXEC; DIG 1; EXEC ] ] ] in
  Alcotest.(check string)
    "compile (fun f x -> x f x)" (to_string expected) (to_string result)

let cases =
  let open Alcotest in
  ( "Pair Compilation"
  , [
      test_case "compile O1" `Quick compile_01
    ; test_case "compile 02" `Quick compile_02
    ] )