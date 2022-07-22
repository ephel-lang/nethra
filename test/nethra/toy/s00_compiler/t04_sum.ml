open Common
open Nethra.Toy.Compiler

let compile_basic_sum () =
  let open Preface_stdlib.Result.Functor (struct
    type t = string
  end) in
  let result =
    Pass.run
      {toy|
        --- Preamble
        sig int  : type
        sig char : type
        ------------
        sig ic : int | char -> type
        def ic = (x).case x (_).char (_).int

        sig m1 : ic (inl 1)
        def m1 = 'c'

        sig m2 : ic (inr m1)
        def m2 = 1
        ------------
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string)) "basic sum type" expected result

let compile_recursive_sum () =
  let open Preface_stdlib.Result.Functor (struct
    type t = string
  end) in
  let result =
    Pass.run
      {toy|
        --- Preamble
        sig unit : type
        sig Unit : unit
        sig int  : type
        ------------
        sig list : (type) -> type
        def list = (X).rec(l:type).(unit | X * l)

        sig Nil  : {X:type} -> list X
        def Nil  = fold inl Unit

        sig Cons : {X:type} -> X -> list X -> list X
        def Cons = (X).(l).fold inr (X,l)

        sig test : list int
        def test = Cons 1 (Nil {int})

        sig append : {X:type} -> list X -> list X -> list X
        def append = (l1).(l2).case (unfold l1) (_).l2 (c).(Cons (fst c) (append (snd c) l2))
        ------------
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string)) "recursive sum type" expected result

let compile_recursive_sum_with_pseudo_constructors () =
  let open Preface_stdlib.Result.Functor (struct
    type t = string
  end) in
  let result =
    Pass.run
      {toy|
        --- Preamble
        sig int   : type
        ------------
        sig nilT  : type
        sig Nil   : nilT
        sig consT : type
        sig Cons  : consT

        sig list : (type) -> type
        def list = (X).rec(l:type).(nilT | consT * X * l)

        sig nil : {X:type} -> list X
        def nil = fold inl Nil

        sig cons : {X:type} -> X -> list X -> list X
        def cons = (h).(l).fold inr (Cons,h,l)

        sig test : list int
        def test = cons 1 (nil {int})

        sig append : {X:type} -> list X -> list X -> list X
        def append = (l1).(l2).case (unfold l1) (_).l2 (c).(cons (fst (snd c)) (append (snd (snd c)) l2))
        ------------
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string))
    "recursive sum type with pseudo constructors" expected result

let compile_peano () =
  let open Preface_stdlib.Result.Functor (struct
    type t = string
  end) in
  let result =
    Pass.run
      {toy|
        ------------
        sig zeroT : type
        sig Zero  : zeroT
        sig succT : type
        sig Succ  : succT

        sig peano : type
        def peano = rec(p:type).(zeroT | succT * p)

        sig zero : peano
        def zero = fold inl Zero

        sig succ : peano -> peano
        def succ = (p).fold inr (Succ,p)

        sig add : peano -> peano -> peano
        def add = (p1).(p2).case (unfold p1) (_).p2 (p1).(succ (add (snd p1) p2))
        ------------
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string)) "recursive peano type" expected result

let compile_reflexivity () =
  let open Preface_stdlib.Result.Functor (struct
    type t = string
  end) in
  let result =
    Pass.run
      {toy|
        -{
            In Agda the reflexivity is expressed thanks to the GADT:
            ```agda
            data _≡_ {A : Set} (x : A) : A → Set where
                refl : x ≡ x
            ```
        }-

        sig reflT : type
        sig Refl  : reflT

        sig eq : {A:type} -> A -> A -> type
        def eq = {A}.(a).(b).(reflT * eq {A} a a)
        -{
            Hum, this example is not really conclusive
        }-
        |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string)) "reflexivity" expected result

let compile_either () =
  let open Preface_stdlib.Result.Functor (struct
    type t = string
  end) in
  let result =
    Pass.run
      {toy|
        -{
            Simulate atoms for constructor definition
        }-
        sig rightT : type
        sig Right  : rightT
        sig leftT  : type
        sig Left   : leftT

        sig either : (type) -> (type) -> type
        def either = (A).(B).((leftT * A) | (rightT * B))

        sig left   : {A:type} -> {B:type} -> A -> either A B
        def left   = (a).inl (Left, a)

        sig right  : {A:type} -> {B:type} -> B -> either A B
        def right  = (b).inr (Right, b)

        sig map    : {A:type} -> {B:type} -> {C:type} -> (B -> C) -> either A B -> either A C
        def map    = (f).(e).case e (e).(inl e) (e).(inr (fst e,f (snd e)))

        sig apply  : {A:type} -> {B:type} -> {C:type} -> either A (B -> C) -> either A B -> either A C
        def apply  = (f).(e).case f (f).(inl f) (f).(map (snd f) e)

        sig join   : {A:type} -> {B:type} -> either A (either A B) -> either A B
        def join   = (e).case e (e).(inl e) (e).(snd e)

        sig bind   : {A:type} -> {B:type} -> {C:type} -> (B -> either A C) -> either A B -> either A C
        def bind   = (f).(e).(join (map f e))
        |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string)) "either type" expected result

let cases =
  let open Alcotest in
  ( "Sum Compiler"
  , [
      test_case "basic sum type" `Quick compile_basic_sum
    ; test_case "recursive sum type" `Quick compile_recursive_sum
    ; test_case "recursive sum type with pseudo constructors" `Quick
        compile_recursive_sum_with_pseudo_constructors
    ; test_case "recursive peano type" `Quick compile_peano
    ; test_case "reflexivity" `Quick compile_reflexivity
    ; test_case "either type" `Quick compile_either
    ] )