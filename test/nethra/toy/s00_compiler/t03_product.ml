open Common
open Nethra.Toy.Compiler

let compile_basic_product () =
  let open Preface_stdlib.Result.Functor (struct
    type t = string
  end) in
  let result =
    Stage.run
      {toy|
        sig Unit : type
        sig unit : Unit
        ------------
        sig pair : (X:type) * X
        def pair = (Unit, unit)
        ------------
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string)) "basic product type" expected result

let compile_basic_product_fails () =
  let open Preface_stdlib.Result.Functor (struct
    type t = string
  end) in
  let result =
    Stage.run
      {toy|
        sig Unit : type
        sig unit : Unit
        ------------
        sig pair : (X:type) * X
        def pair = (Unit, 1)
        ------------
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok false in
  Alcotest.(check (result bool string))
    "basic product type fails" expected result

let compile_basic_product_first () =
  let open Preface_stdlib.Result.Functor (struct
    type t = string
  end) in
  let result =
    Stage.run
      {toy|
        sig Unit : type
        sig unit : Unit
        ------------
        sig pair : (X:type) * X
        def pair = (Unit, unit)

        sig first : type
        def first = fst pair
        ------------
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string)) "basic product first" expected result

let compile_basic_product_second () =
  let open Preface_stdlib.Result.Functor (struct
    type t = string
  end) in
  let result =
    Stage.run
      {toy|
        sig Unit : type
        sig unit : Unit
        ------------
        sig pair : (X:type) * X
        def pair = (Unit, unit)

        sig second : Unit
        def second = snd pair
        ------------
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string)) "basic product second" expected result

let compile_trait_denotation () =
  let open Preface_stdlib.Result.Functor (struct
    type t = string
  end) in
  let result =
    Stage.run
      {toy|
        -{
            trait Monoid {
                sig t       : type
                sig empty   : t
                sig compose : t -> t -> t
            }
        }-

        sig Monoid : type
        def Monoid = (t:type) * (t * (t -> t -> t))

        sig Empty : type
        def Empty = (t:type) * t

        sig Compose : type
        def Compose = (t:type) * (t -> t -> t)

        sig empty : Monoid -> Empty
        def empty = (x).(fst x, fst (snd x))

        sig compose : Monoid -> Compose
        def compose = (x).(fst x, snd (snd x))
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string)) "trait denotation" expected result

let compile_trait_implementation () =
  let open Preface_stdlib.Result.Functor (struct
    type t = string
  end) in
  let result =
    Stage.run
      {toy|
        -{
            trait Monoid {
                sig empty   : t
                sig compose : t -> t -> t
            }
        }-

        sig Monoid : type
        def Monoid = (t:type) * (t * (t -> t -> t))

        sig Empty : type
        def Empty = (t:type) * t

        sig Compose : type
        def Compose = (t:type) * (t -> t -> t)

        sig empty : Monoid -> Empty
        def empty = (x).(fst x, fst (snd x))

        sig compose : Monoid -> Compose
        def compose = (x).(fst x, snd (snd x))

        ------------

        -{
            impl Monoid {
                def t       = int
                def empty   = 0
                def compose = add
            }
        }-

        sig int : type
        sig add : int -> int -> int

        sig IntMonoid : Monoid
        def IntMonoid = (int, 0, add)
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string)) "trait implementation" expected result

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
