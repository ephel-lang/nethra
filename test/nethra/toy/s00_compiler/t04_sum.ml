open Common
open Nethra.Toy.Compiler

open Preface_stdlib.Result.Functor (struct
  type t = Nethra.Syntax.Source.Region.t Pass.error
end)

let compile_basic_sum () =
  let result =
    Pass.run
      {toy|
        --- Preamble
        sig int  : type
        sig char : type
        ------------
        sig ic : int | char -> type
        val ic = (x).case x (_).char (_).int

        sig m1 : ic (inl 1) -- char
        val m1 = 'c'

        sig m2 : ic (inr m1)
        val m2 = 1
        ------------
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string))
    "basic sum type" expected (string_of_error result)

let compile_bool () =
  let result =
    Pass.run
      {toy|
        --- Preamble
        sig Unit : type
        sig unit : Unit
        ------------
        sig Bool : type
        val Bool = Unit | Unit

        sig true  : Bool
        val true  = inl unit

        sig false  : Bool
        val false  = inr unit

        sig Test : Bool -> type
        val Test = (a).case a (_).Unit (_).Bool

        sig test : (b:Bool) -> Test b
        val test = (c).case c (_).unit (_).true
        ------------
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string))
    "dependant type and case" expected (string_of_error result)

let compile_recursive_sum () =
  let result =
    Pass.run
      {toy|
        --- Preamble
        sig unit : type
        sig Unit : unit
        sig int  : type
        ------------
        sig list : (type) -> type
        val list = (X).rec(l:type).(unit | X * l)

        sig Nil  : {X:type} -> list X
        val Nil  = fold inl Unit

        sig Cons : {X:type} -> X -> list X -> list X
        val Cons = (X).(l).fold inr (X,l)

        sig test : list int
        val test = Cons 1 (Nil {int})

        sig append : {X:type} -> list X -> list X -> list X
        val append = (l1).(l2).case (unfold l1) (_).l2 (c).(Cons (fst c) (append (snd c) l2))
        ------------
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string))
    "recursive sum type" expected (string_of_error result)

let compile_recursive_sum_with_pseudo_constructors () =
  let result =
    Pass.run
      {toy|
        --- Preamble
        sig int   : type
        sig string : type
        sig Atom   : (a:string) -> type
        sig data   : {A:type} -> (_:A) -> type
        val data   = {A}.(_).A
        ------------
        sig Nil   : Atom "Nil"
        sig Cons  : Atom "Cons"

        sig list : (type) -> type
        val list = (X).rec(l:type).(data Nil | data Cons * X * l)

        sig nil : {X:type} -> list X
        val nil = fold inl Nil

        sig cons : {X:type} -> X -> list X -> list X
        val cons = (h).(l).fold inr (Cons,h,l)

        sig test : list int
        val test = cons 1 (nil {int})

        sig append : {X:type} -> list X -> list X -> list X
        val append = (l1).(l2).case (unfold l1) (_).l2 (c).(cons (fst (snd c)) (append (snd (snd c)) l2))
        ------------
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string))
    "recursive sum type with pseudo constructors" expected
    (string_of_error result)

let compile_peano () =
  let result =
    Pass.run
      {toy|
        --- Preamble
        sig string : type
        sig Atom   : (a:string) -> type
        sig data   : {A:type} -> (_:A) -> type
        val data   = {A}.(_).A
        ------------
        sig Zero : Atom "Zero"
        sig Succ : Atom "Succ"

        sig peano : type
        val peano = rec(p:type).(data Zero | data Succ * p)

        sig zero : peano
        val zero = fold inl Zero

        sig succ : peano -> peano
        val succ = (p).fold inr (Succ,p)

        sig add : peano -> peano -> peano
        val add = (p1).(p2).case (unfold p1) (_).p2 (p1).(succ (add (snd p1) p2))
        ------------
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string))
    "recursive peano type" expected (string_of_error result)

let compile_either () =
  let result =
    Pass.run
      {toy|
        -{
            Simulate atoms for constructor definition
        }-
        --- Preamble
        sig string : type
        sig Atom   : string -> type
        sig data   : {A:type} -> (_:A) -> type
        val data   = {A}.(_).A
        ------------
        sig Right : Atom "Right"
        sig Left  : Atom "Left"

        sig either : (type) -> (type) -> type
        val either = (A).(B).((data Left * A) | (data Right * B))

        sig left   : {A:type} -> {B:type} -> A -> either A B
        val left   = (a).inl (Left, a)

        sig right  : {A:type} -> {B:type} -> B -> either A B
        val right  = (b).inr (Right, b)

        sig map    : {A:type} -> {B:type} -> {C:type} -> (B -> C) -> either A B -> either A C
        val map    = (f).(e).case e (e).(left (snd e)) (e).(right (f (snd e)))

        sig apply  : {A:type} -> {B:type} -> {C:type} -> either A (B -> C) -> either A B -> either A C
        val apply  = (f).(e).case f (f).(left (snd f)) (f).(map (snd f) e)

        sig join   : {A:type} -> {B:type} -> either A (either A B) -> either A B
        val join   = (e).case e (e).(left (snd e)) (e).(snd e)

        sig bind   : {A:type} -> {B:type} -> {C:type} -> (B -> either A C) -> either A B -> either A C
        val bind   = (f).(e).(join (map f e))
        |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string))
    "either type" expected (string_of_error result)

let compile_freer () =
  let result =
    Pass.run
      {toy|
            -{
                Simulate atoms for constructor definition
            }-
            --- Preamble
            sig string : type
            sig Atom   : string -> type
            sig data   : {A:type} -> (_:A) -> type
            val data   = {A}.(_).A
            ------------
            sig Return : Atom "Return"
            sig Bind   : Atom "Bind"
    
            sig Free : ((type) -> type) -> (type) -> type
            val Free = (F).(A).rec(Free:type).((data Return * A) | (data Bind * F Free))

            sig return : {F:(type)->type} -> {A:type} -> A -> Free F A 
            val return = (a).fold inl (Return, a)

            sig bind : {F:(type)->type} -> {A:type} -> F (Free F A) -> Free F A 
            val bind = (a).fold inr (Bind, a)

            sig map_a : {F:(type)->type} -> {A:type} -> {B:type} -> (A -> B ) -> F A -> F B
            
            sig map : {F:(type)->type} -> {A:type} -> {B:type} -> (A -> B) -> Free F A -> Free F B
            val map = (f).(a).case (unfold a) (a).(return (f (snd a))) (a).(bind (map_a (map f) (snd a)))
            |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string))
    "either type" expected (string_of_error result)

let cases =
  let open Alcotest in
  ( "Sum Compiler"
  , [
      test_case "basic sum type" `Quick compile_basic_sum
    ; test_case "bool type" `Quick compile_bool
    ; test_case "recursive sum type" `Quick compile_recursive_sum
    ; test_case "recursive sum type with pseudo constructors" `Quick
        compile_recursive_sum_with_pseudo_constructors
    ; test_case "recursive peano type" `Quick compile_peano
    ; test_case "either type" `Quick compile_either
    ; test_case "freer type" `Quick compile_freer
    ] )