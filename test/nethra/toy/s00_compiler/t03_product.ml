open Common
open Nethra.Toy.Compiler

open Preface_stdlib.Result.Functor (struct
  type t = Nethra.Syntax.Source.Region.t Pass.error
end)

let compile_basic_product () =
  let result =
    Pass.run
      {toy|
        sig Unit : type
        sig unit : Unit
        ------------
        sig pair : (X:type) * X
        val pair = (Unit, unit)
        ------------
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string))
    "basic product type" expected (string_of_error result)

let compile_basic_product_fails () =
  let result =
    Pass.run
      {toy|
        sig Unit : type
        sig unit : Unit
        ------------
        sig pair : (X:type) * X
        val pair = (Unit, 1)
        ------------
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok false in
  Alcotest.(check (result bool string))
    "basic product type fails" expected (string_of_error result)

let compile_basic_product_first () =
  let result =
    Pass.run
      {toy|
        sig Unit : type
        sig unit : Unit
        ------------
        sig pair : (X:type) * X
        val pair = (Unit, unit)

        sig first : type
        val first = fst pair
        ------------
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string))
    "basic product first" expected (string_of_error result)

let compile_basic_product_second () =
  let result =
    Pass.run
      {toy|
        sig Unit : type
        sig unit : Unit
        ------------
        sig pair : (X:type) * X
        val pair = (Unit, unit)

        sig second : Unit
        val second = snd pair
        ------------
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string))
    "basic product second" expected (string_of_error result)

let compile_trait_denotation () =
  let result =
    Pass.run
      {toy|
        -{
            trait Monoid {
                sig t       : type
                sig empty   : t
                sig compose : t -> t -> t
            }
        }-

        sig Monoid : type
        val Monoid = (t:type) * (t * (t -> t -> t))

        sig Empty : type
        val Empty = (t:type) * t

        sig Compose : type
        val Compose = (t:type) * (t -> t -> t)

        sig empty : Monoid -> Empty
        val empty = (x).(fst x, fst (snd x))

        sig compose : Monoid -> Compose
        val compose = (x).(fst x, snd (snd x))
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string))
    "trait denotation" expected (string_of_error result)

let compile_trait_implementation () =
  let result =
    Pass.run
      {toy|
        -{
            trait Monoid {
                sig empty   : self
                sig compose : self -> self -> self
            }
        }-

        sig Monoid : type
        val Monoid = (t:type) * (t * (t -> t -> t))

        sig Empty : type
        val Empty = (t:type) * t

        sig Compose : type
        val Compose = (t:type) * (t -> t -> t)

        sig empty : Monoid -> Empty
        val empty = (x).(fst x, fst (snd x))

        sig compose : Monoid -> Compose
        val compose = (x).(fst x, snd (snd x))

        ------------

        -{
            impl Monoid for int {
                val empty   = 0
                val compose = add
            }
        }-

        sig int : type
        sig add : int -> int -> int

        sig Monoid_for_Int : Monoid
        val Monoid_for_Int = (int, 0, add)
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string))
    "trait implementation" expected (string_of_error result)

let cases =
  let open Alcotest in
  ( "Product Compiler"
  , [
      test_case "basic product type" `Quick compile_basic_product
    ; test_case "basic product type fails" `Quick compile_basic_product_fails
    ; test_case "basic product first" `Quick compile_basic_product_first
    ; test_case "basic product second" `Quick compile_basic_product_second
    ; test_case "trait denotation" `Quick compile_trait_denotation
    ; test_case "trait implementation" `Quick compile_trait_implementation
    ] )
