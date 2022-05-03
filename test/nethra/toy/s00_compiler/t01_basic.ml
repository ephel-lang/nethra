open Nethra.Toy.Compiler
open Nethra.Lang.Ast.Proof
open Nethra.Lang.Render.Proof

let rec check = function
  | [] -> true
  | (_, None) :: _ -> false
  | (_, Some proof) :: l ->
    if is_success proof
    then check l
    else
      let _ = render Format.std_formatter proof in
      check l && false

let compile_basic_int () =
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

let cases =
  let open Alcotest in
  ("Basic Compiler", [ test_case "basic int" `Quick compile_basic_int ])
