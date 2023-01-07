open Poc.Expr
open Poc.VM
open Poc

let compile_01 () =
  let result = compile (Abs ("x", Var "x"))
  and expected = [ SEQ [] ] in
  Alcotest.(check string) "compile fun x -> x" (render expected) (render result)

let compile_02 () =
  let result = compile (Abs ("x", Unit))
  and expected = [ SEQ [ DROP 1; PUSH UNIT ] ] in
  Alcotest.(check string)
    "compile fun x -> unit" (render expected) (render result)

let compile_03 () =
  let result = compile (App (Abs ("x", Var "x"), Int 1))
  and expected = [ PUSH (INT 1) ] in
  Alcotest.(check string)
    "compile (fun x -> x) 1" (render expected) (render result)

let compile_04 () =
  let result = compile (App (Abs ("x", Unit), Int 1))
  and expected = [ PUSH UNIT ] in
  Alcotest.(check string)
    "compile (fun x -> unit) 1" (render expected) (render result)

let compile_05 () =
  let result = compile (App (App (Abs ("x", Abs ("y", Var "y")), Int 1), Int 1))
  and expected = [ PUSH (INT 1) ] in
  Alcotest.(check string)
    "compile (fun x y -> y) 1 1" (render expected) (render result)

let cases =
  let open Alcotest in
  ( "Basic Compilation"
  , [
      test_case "compile O1" `Quick compile_01
    ; test_case "compile O2" `Quick compile_02
    ; test_case "compile O3" `Quick compile_03
    ; test_case "compile O4" `Quick compile_04
    ; test_case "compile O5" `Quick compile_05
    ] )