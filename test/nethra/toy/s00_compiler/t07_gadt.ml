open Common
open Nethra.Toy.Compiler

open Preface_stdlib.Result.Functor (struct
  type t = Nethra.Syntax.Source.Region.t Pass.error
end)

let compile_gadt () =
  let result =
    Pass.run
      {toy|
      -- Preamble

      sig Unit : type
      sig unit : Unit

      val Bool  : type = Unit | Unit
      val true  : Bool = inl unit
      val false : Bool = inr unit

      sig int  : type

     -{
         data Expr A =
         | Boolean  of Bool with A = Bool
         | Integer  of int  with A = int
     }-

      sig Expr : (type) -> type
      val Expr = (A).((Bool * equals A Bool) | (int * equals A int))

      sig boolean : {A:type} -> {_:equals A Bool} -> Bool -> Expr A
      val boolean = {_ p}.(b).inl (b,p)

      sig number  : {A:type} -> {_:equals A int}  -> int  -> Expr A
      val number  = {_ p}.(b).inr (b,p)

      sig id_subst : {A B:type} -> (A * equals B A) -> B
      val id_subst = (e).(subst fst e by snd e)

      sig eval : {A:type} -> Expr A -> A
      val eval = {A}.(e).case e (id_subst {Bool} {A}) (id_subst {int} {A})

      val res1 : int  = eval (number 1)
      val res2 : Bool = eval (boolean true)
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string)) "GADT" expected (string_of_error result)

let cases =
  let open Alcotest in
  ("GADT Compiler", [ test_case "GADT" `Quick compile_gadt ])
