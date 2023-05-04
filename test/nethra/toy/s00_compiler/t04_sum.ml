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
        val ic = fun x -> case x (fun _ -> char) (fun _ -> int)

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

let compile_int_int_sum () =
  let result =
    Pass.run
      {toy|
        --- Preamble
        sig int : type
        sig add : int -> int -> int
        ------------
        val ic : type = int | int

        val m1 : int -> ic = fun a -> inl a
        val m2 : int -> ic = fun a -> inr a

        val get : ic -> int = fun ic -> case ic (add 2) (add 1)

        val GetL : {x:int} -> equals (get (m1 x)) (add 2 x) = refl
        val GetR : {x:int} -> equals (get (m2 x)) (add 1 x) = refl
        ------------
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string))
    "basic int | int type" expected (string_of_error result)

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
        val Test = fun a -> case a (fun _ -> Unit) (fun _ -> Bool)

        sig test : (b:Bool) -> Test b
        val test = fun c -> case c (fun _ -> unit) (fun _ -> true)
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
        val list = fun X -> rec(l:type).(unit | X * l)

        sig Nil  : {X:type} -> list X
        val Nil  = fold inl Unit

        sig Cons : {X:type} -> X -> list X -> list X
        val Cons = fun X l -> fold inr (X,l)

        sig test : list int
        val test = Cons 1 (Nil {int})

        sig append : {X:type} -> list X -> list X -> list X
        val append = fun l1 l2 -> case (unfold l1) (fun _ -> l2) (fun c -> Cons (fst c) (append (snd c) l2))
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
        val data   = fun {A} _ -> A
        ------------
        sig Nil   : Atom "Nil"
        sig Cons  : Atom "Cons"

        sig list : (type) -> type
        val list = fun X -> rec(l:type).(data Nil | data Cons * X * l)

        sig nil : {X:type} -> list X
        val nil = fold inl Nil

        sig cons : {X:type} -> X -> list X -> list X
        val cons = fun h l -> fold inr (Cons,h,l)

        sig test : list int
        val test = cons 1 (nil {int})

        sig append : {X:type} -> list X -> list X -> list X
        val append = fun l1 l2 -> case (unfold l1) (fun _ -> l2) (fun c -> cons (fst (snd c)) (append (snd (snd c)) l2))
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
        val data   = fun {A} _ -> A
        ------------
        sig Zero : Atom "Zero"
        sig Succ : Atom "Succ"

        sig peano : type
        val peano = rec(p:type).(data Zero | data Succ * p)

        sig zero : peano
        val zero = fold inl Zero

        sig succ : peano -> peano
        val succ = fun p -> fold inr (Succ,p)

        sig add : peano -> peano -> peano
        val add = rec(add:peano -> peano -> peano).fun p1 p2 -> case (unfold p1) (fun _ -> p2) (fun p1 -> succ (add (snd p1) p2))
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
        sig data   : {A:type} -> A -> type
        val data   = fun {A} _ -> A
        ------------
        sig Right : Atom "Right"
        sig Left  : Atom "Left"

        sig either : (type) -> (type) -> type
        val either = fun A B -> (data Left * A) | (data Right * B)

        sig left   : {A B:type} -> A -> either A B
        val left   = fun a -> inl (Left, a)

        sig right  : {A B:type}  -> B -> either A B
        val right  = fun b -> inr (Right, b)

        sig map    : {A B C:type} -> (B -> C) -> either A B -> either A C
        val map    = fun f e -> case e (fun e -> left (snd e)) (fun e -> right (f (snd e)))

        sig apply  : {A B C:type} -> either A (B -> C) -> either A B -> either A C
        val apply  = fun f e -> case f (fun f -> left (snd f)) (fun f -> map (snd f) e)

        sig join   : {A B:type} -> either A (either A B) -> either A B
        val join   = fun e -> case e (fun e -> left (snd e)) (fun e -> snd e)

        sig bind   : {A B C:type} -> (B -> either A C) -> either A B -> either A C
        val bind   = fun f e -> join (map f e)
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
            sig data   : {A:type} -> A -> type
            val data   = fun {A} _ -> A
            ------------
            sig Return : Atom "Return"
            sig Bind   : Atom "Bind"
    
            sig Free : ((type) -> type) -> (type) -> type
            val Free = fun F A -> rec(Free:type).((data Return * A) | (data Bind * F Free))

            sig return : {F:(type)->type} -> {A:type} -> A -> Free F A 
            val return = fun a -> fold inl (Return, a)

            sig bind : {F:(type)->type} -> {A:type} -> F (Free F A) -> Free F A 
            val bind = fun a -> fold inr (Bind, a)

            sig map_a : {F:(type)->type} -> {A B:type} -> (A -> B ) -> F A -> F B
            
            sig map : {F:(type)->type} -> {A B :type} -> (A -> B) -> Free F A -> Free F B
            val map = fun f a -> case (unfold a) (fun a -> return (f (snd a))) (fun a -> bind (map_a (map f) (snd a)))
            |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string))
    "freer type" expected (string_of_error result)

let cases =
  let open Alcotest in
  ( "Sum Compiler"
  , [
      test_case "basic sum type" `Quick compile_basic_sum
    ; test_case "basic int | int  type" `Quick compile_int_int_sum
    ; test_case "bool type" `Quick compile_bool
    ; test_case "recursive sum type" `Quick compile_recursive_sum
    ; test_case "recursive sum type with pseudo constructors" `Quick
        compile_recursive_sum_with_pseudo_constructors
    ; test_case "recursive peano type" `Quick compile_peano
    ; test_case "either type" `Quick compile_either
    ; test_case "freer type" `Quick compile_freer
    ] )
