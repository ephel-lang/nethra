open Common
open Nethra.Toy.Compiler

open Preface_stdlib.Result.Functor (struct
  type t = Nethra.Syntax.Source.Region.t Pass.error
end)

let compile_basic_record () =
  let result =
    Pass.run
      {toy|
        -----------
        sig int : type
        -----------
        sig point :type

        val point =
            sig struct
                x : int
                y : int
            end

         sig zero : point
         val zero =
            val struct
                x = 0
                y = 0
            end
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string))
    "Basic record" expected (string_of_error result)

let compile_parametric_record () =
  let result =
    Pass.run
      {toy|
        -----------
        sig int : type
        sig add : int -> int -> int
        -----------
        sig monoid : (type) -> type
        val monoid =
            (t).sig struct
                initial : t
                combine : (t -> t -> t)
            end

        sig integer : monoid int
        val integer =
            val struct
                initial = 0
                combine = add
            end

        sig zero : int
        val zero = initial from integer
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string))
    "Parametric record" expected (string_of_error result)

let compile_recursive_record () =
  let result =
    Pass.run
      {toy|
        -----------
        sig int : type
        sig add : int -> int -> int
        -----------

        val point : type =
            let t = int in
                rec(self:type).sig struct
                    x  : t
                    y  : t
                    mv : (self -> (t * t) -> self)
                end

        val Point : int -> int -> point = (x).(y).
            fold val struct
                x  = x
                y  = y
                mv = (self).(x_and_y).
                    let nx = add (fst x_and_y) (x from unfold self) in
                    let ny = add (snd x_and_y) (y from unfold self) in
                    (Point nx ny)
            end

        val zero : point = Point 0 0

        val x : int = x from unfold zero
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string))
    "Recursive record" expected (string_of_error result)

let compile_monad_record () =
  let result =
    Pass.run
      {toy|
        ------------
        sig Unit : type
        sig unit : Unit
        ------------

        sig Monad : ((type) -> type) -> type
        val Monad =
            (M).sig struct
                map   : ({A:type} -> {B:type} -> (A -> B) -> M A -> M B)
                apply : ({A:type} -> {B:type} -> M (A -> B) -> M A -> M B)
                join  : ({A:type} -> M (M A) -> M A)
                bind  : ({A:type} -> {B:type} -> (A -> M B) -> M A -> M B)
            end

        ------------
        val Option : (type) -> type = (A).(A | Unit)

        sig some : {A:type} -> A -> Option A
        val some = (a).inl a

        sig none : {A:type} -> Option A
        val none = inr unit

        sig EitherOption : Monad Option
        val EitherOption =
            rec(S:Monad Option).val struct
                map   = {_}.{B}.(f).(ma).(case ma (a).(some (f a)) (_).(none {B}))
                apply = {_}.{B}.(mf).(ma).(case mf (f).(map from S f ma) (_).(none {B}))
                join  = {A}.(ma).(case ma (a).a (_).(none {A}))
                bind  = (f).(ma).(join from S (map from S f ma))
            end

        val r : Option Unit = map from EitherOption (_).unit (some 1)
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string))
    "Monad record" expected (string_of_error result)

let cases =
  let open Alcotest in
  ( "Record Compiler"
  , [
      test_case "Basic Record" `Quick compile_basic_record
    ; test_case "Parametric Record" `Quick compile_parametric_record
    ; test_case "Recursive Record" `Quick compile_recursive_record
    ; test_case "Monad Record" `Quick compile_monad_record
    ] )