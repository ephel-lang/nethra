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

let cases =
  let open Alcotest in
  ( "Record Compiler"
  , [
      test_case "Basic Record" `Quick compile_basic_record
    ; test_case "Parametric Record" `Quick compile_parametric_record
    ; test_case "Recursive Record" `Quick compile_recursive_record
    ] )
