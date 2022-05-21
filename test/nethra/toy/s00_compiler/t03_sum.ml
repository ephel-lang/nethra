open Common
open Nethra.Toy.Compiler

let compile_basic_sum () =
  let open Preface_stdlib.Result.Functor (struct
    type t = string
  end) in
  let result =
    Stage.run
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
    Stage.run
      {toy|
        --- Preamble
        sig unit : type
        sig Unit : unit
        sig int  : type
        ------------
        sig list : (type) -> type
        def list = (X).rec(l).(unit | X * l)

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
    Stage.run
      {toy|
        --- Preamble
        sig int  : type
        ------------
        sig nilT  : type
        sig Nil   : nilT
        sig consT : type
        sig Cons  : consT

        sig list : (type) -> type
        def list = (X).rec(l).(nilT | consT * X * l)

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
  Alcotest.(check (result bool string)) "recursive sum type with pseudo constructors" expected result

let compile_peano () =
  let open Preface_stdlib.Result.Functor (struct
    type t = string
  end) in
  let result =
    Stage.run
      {toy|
        ------------
        sig zeroT  : type
        sig Zero   : zeroT
        sig succT : type
        sig Succ  : succT

        sig peano : type
        def peano = rec(p).(zeroT | succT * p)

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
    Stage.run
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
        def eq = {A}.(a).(_).(reflT * eq {A} a a)
        |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string)) "reflexivity" expected result

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
    ] )
