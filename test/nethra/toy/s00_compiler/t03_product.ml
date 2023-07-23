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
        val empty = fun x -> fst x, fst (snd x)

        sig compose : Monoid -> Compose
        val compose = fun x -> fst x, snd (snd x)
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
        val empty = fun x -> fst x, fst (snd x)

        sig compose : Monoid -> Compose
        val compose = fun x -> fst x, snd (snd x)

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

let compile_high_rank_polymorphism () =
  let result =
    Pass.run
      {toy|
        ------------
        sig int : type
        sig string : type
        ------------

        sig f : ({a:type} -> a -> a) -> (int * string)
        val f = fun g -> g 1, g "2"
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string))
    "high rank polymorphism" expected (string_of_error result)

let compile_nat_definition () =
  let result =
    Pass.run
      {toy|
        sig int : type
        sig unit : type
        sig `()` : unit
        ------------
        val `?`  : type -> type = fun t -> t | unit
        val some : {a:type} -> a -> `?` a = fun a -> inl a
        val none : {a:type} -> `?` a = inr `()`
        ------------
        sig `>=` : int -> int -> type
        ------------
        sig nat : type
        val nat = (x:int) * (`>=` x 0)
        sig is_nat : (x: int) -> `?` (`>=` x 0)
        ------------
        sig nat_to_int : nat -> int
        val nat_to_int = fun n -> fst n
        ------------
        sig int_to_nat : (x:int) -> {p:`>=` x 0} -> nat
        val int_to_nat = fun x {p} -> (x,p)
        ------------
        sig int_to_nat_opt : (x:int) -> `?` nat
        val int_to_nat_opt = fun x ->
                case (is_nat x)
                     (fun p -> some {nat} (x,p))
                     (fun u -> none {nat})
        ------------
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string))
    "nat definition" expected (string_of_error result)

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
    ; test_case "high rank polymorphism" `Quick compile_high_rank_polymorphism
    ; test_case "nat defintion" `Quick compile_nat_definition
    ] )
