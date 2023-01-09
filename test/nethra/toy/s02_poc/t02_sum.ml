open Expr
open Vm
open Compiler

let compile_01 () =
  let result = optimise @@ compile (Inl (Int 1))
  and expected = [ PUSH (INT 1); LEFT ] in
  Alcotest.(check string)
    "compile Inl 1" (to_string expected) (to_string result)

let compile_02 () =
  let result = optimise @@ compile (Inr (Int 1))
  and expected = [ PUSH (INT 1); RIGHT ] in
  Alcotest.(check string)
    "compile Inr 1" (to_string expected) (to_string result)

let compile_03 () =
  let result =
    optimise @@ compile (Case (Inl (Int 1), "x", Var "x", "x", Var "x"))
  and expected = [ PUSH (INT 1) ] in
  Alcotest.(check string)
    "compile case (inl 1) (fun x -> x) (fun x -> x)" (to_string expected)
    (to_string result)

let compile_04 () =
  let result =
    optimise @@ compile (Case (Inr (Int 1), "x", Var "x", "x", Var "x"))
  and expected = [ PUSH (INT 1) ] in
  Alcotest.(check string)
    "compile case (inr 1) (fun x -> x) (fun x -> x)" (to_string expected)
    (to_string result)

let compile_05 () =
  let result = optimise @@ compile (Case (Inl (Int 1), "x", Int 2, "x", Var "x"))
  and expected = [ PUSH (INT 2) ] in
  Alcotest.(check string)
    "compile case (inl 1) (fun x -> 2) (fun x -> x)" (to_string expected)
    (to_string result)

let compile_06 () =
  let result = optimise @@ compile (Case (Inr (Int 1), "x", Var "x", "x", Int 2))
  and expected = [ PUSH (INT 2) ] in
  Alcotest.(check string)
    "compile case (inr 1) (fun x -> x) (fun x -> 2)" (to_string expected)
    (to_string result)

let compile_07 () =
  let result =
    optimise
    @@ compile
         (Case
            ( Inl (Inr (Int 1))
            , "x"
            , Case (Var "x", "x", Var "x", "x", Int 2)
            , "x"
            , Int 2 ) )
  and expected = [ PUSH (INT 2) ] in
  Alcotest.(check string)
    "compile case (inr 1) (fun x -> x) (fun x -> 2)" (to_string expected)
    (to_string result)

let compile_08 () =
  let result = optimise @@ compile (Case (Inl (Int 1), "x", Unit, "x", Var "x"))
  and expected = [ PUSH UNIT ] in
  Alcotest.(check string)
    "compile case (inl 1) (fun x -> unit) (fun x -> x)" (to_string expected)
    (to_string result)

let cases =
  let open Alcotest in
  ( "Pair Compilation"
  , [
      test_case "compile O1" `Quick compile_01
    ; test_case "compile O2" `Quick compile_02
    ; test_case "compile O3" `Quick compile_03
    ; test_case "compile O4" `Quick compile_04
    ; test_case "compile O5" `Quick compile_05
    ; test_case "compile O6" `Quick compile_06
    ; test_case "compile O7" `Quick compile_07
    ; test_case "compile O8" `Quick compile_08
    ] )