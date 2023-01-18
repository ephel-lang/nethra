open Expr
open Vm
open Compiler
open Expander
open Optimiser
open Simplifier
open Normaliser

let compile s = s |> compile |> expand |> optimise |> simplify |> normalise

let compile_01 () =
  let result = compile (Pair (Int 1, Int 2))
  and expected = [ PUSH (INT 2); PUSH (INT 1); PAIR ] in
  Alcotest.(check string)
    "compile (1,2)" (to_string expected) (to_string result)

let compile_02 () =
  let result = compile (Fst (Pair (Int 1, Int 2)))
  and expected = [ PUSH (INT 1) ] in
  Alcotest.(check string)
    "compile fst (1,2)" (to_string expected) (to_string result)

let compile_03 () =
  let result = compile (Snd (Pair (Int 1, Int 2)))
  and expected = [ PUSH (INT 2) ] in
  Alcotest.(check string)
    "compile snd (1,2)" (to_string expected) (to_string result)

let compile_04 () =
  let result = compile (Abs ("p", Fst (Var "p")))
  and expected = [ LAMBDA ("p", [ CAR ]) ] in
  Alcotest.(check string)
    "compile (fun p -> fst p)" (to_string expected) (to_string result)

let compile_05 () =
  let result = compile (Abs ("p", App (Fst (Var "p"), Snd (Var "p"))))
  and expected = [ LAMBDA ("p", [ DUP (0, "p"); CAR; SWAP; CDR; EXEC ]) ] in
  Alcotest.(check string)
    "compile (fun p -> (fst p) (snd p))" (to_string expected) (to_string result)

let cases =
  let open Alcotest in
  ( "Pair Compilation"
  , [
      test_case "compile O1" `Quick compile_01
    ; test_case "compile 02" `Quick compile_02
    ; test_case "compile 03" `Quick compile_03
    ; test_case "compile 04" `Quick compile_04
    ; test_case "compile 05" `Quick compile_05
    ] )