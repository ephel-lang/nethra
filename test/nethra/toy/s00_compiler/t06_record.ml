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
                sig x : int
                sig y : int
            end

         sig zero : point
         val zero =
            val struct
                val x = 0
                val y = 0
            end
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string))
    "Basic record" expected (string_of_error result)

let compile_basic_dependant_record_type () =
  let result =
    Pass.run
      {toy|
        sig Monoid : type
        val Monoid =
            sig struct
                sig self    : type
                sig neutral : self
                sig combine : self -> self -> self
            end
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string))
    "Basic dependant record type" expected (string_of_error result)

let compile_basic_dependant_record_type_and_instance () =
  let result =
    Pass.run
      {toy|
        -----------
        sig Unit : type
        sig unit : Unit

        -----------

        sig nat : type
        val nat = rec(X:type).(Unit | X)

        val zero : nat = fold inl unit
        val succ : nat -> nat = fun n -> fold inr n

        sig add : nat -> nat -> nat
        val add = fun n1 n2 -> case (unfold n1) (fun _ -> n2) (fun n1 -> add n1 n2)

        -----------

        sig Monoid : type
        val Monoid =
            sig struct
                sig self : type
                sig neutral : self
                sig combine : self -> self -> self
                -- Monoid Laws
                sig law1 : {a:self} -> equals a (combine neutral a)
                -{
                sig law2 : {a:self} -> equals a (combine a neutral)
                sig law3 : {a b c:self} ->
                    let comb = combine in
                    equals (comb (comb a b) c) (comb a (comb b c))
                }-
            end

        -----------

        sig MonoidNat : Monoid
        val MonoidNat =
            val struct
                val self = nat
                val neutral = zero
                val combine = add
                -- Monoid Laws
                val law1 = refl
                -- Additional i.e. private
                val plus_one : nat -> nat = succ
                val one : nat = plus_one (succ neutral)
            end

        sig test : nat
        val test = #neutral MonoidNat
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string))
    "Basic dependant record and instance" expected (string_of_error result)

let compile_parametric_record () =
  let result =
    Pass.run
      {toy|
        -----------
        sig int : type
        sig add : int -> int -> int
        sig string : type
        sig concat : string -> string -> string
        -----------
        sig monoid : (type) -> type
        val monoid =
            fun t -> sig struct
                sig initial : t
                sig combine : t -> t -> t
            end

        sig int_monoid : monoid int
        val int_monoid =
            val struct
                val initial = 0
                val combine = add
            end

        sig zero : int
        val zero = #initial int_monoid

        sig string_monoid : monoid string
        val string_monoid =
            val struct
                val initial = ""
                val combine = concat
            end
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
            rec(self:type).sig struct
                sig x  : int
                sig y  : int
                sig mv : self -> int -> int -> self
            end

        val Point : int -> int -> point =
            fun x y -> fold val struct
                val x  = x
                val y  = y
                val mv = fun self x y ->
                    let nx = add x (#x unfold self) in
                    let ny = add y (#y unfold self) in
                    (Point nx ny)
            end

        val zero : point = Point 0 0

        val x : int = #x unfold zero
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string))
    "Recursive record" expected (string_of_error result)

let compile_monad_dependant_record () =
  let result =
    Pass.run
      {toy|
        ------------
        sig Unit : type
        sig unit : Unit
        ------------

        sig Monad : ((type) -> type) -> type
        val Monad =
            fun M -> sig struct
                sig return : {A:type} -> A -> M A
                sig map    : {A B:type} -> (A -> B) -> M A -> M B
                sig apply  : {A B:type} -> M (A -> B) -> M A -> M B
                sig join   : {A:type} -> M (M A) -> M A
                sig bind   : {A B:type} -> (A -> M B) -> M A -> M B
            end

        ------------
        val Option : (type) -> type = fun A -> A | Unit

        sig some : {A:type} -> A -> Option A
        val some = fun a -> inl a

        sig none : {A:type} -> Option A
        val none = inr unit

        sig EitherOption : Monad Option

        val EitherOption =
            val struct
                val return = some
                val map    = fun {_ B} f ma -> case ma (fun a -> some (f a)) (fun _ -> none {B})
                val apply  = fun {_ B} mf ma -> case mf (fun f -> map f ma) (fun _ -> none {B})
                val join   = fun {A} ma -> case ma (fun a -> a) (fun _ -> none {A})
                val bind   = fun f ma -> join (map f ma)
            end

        val r : Option Unit =
            let m = EitherOption in #map m (fun _ -> unit) (#return m 1)
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string))
    "Monad dependant record" expected (string_of_error result)

let compile_monad_recursive_record () =
  let result =
    Pass.run
      {toy|
        ------------
        sig Unit : type
        sig unit : Unit
        ------------

        sig Monad : ((type) -> type) -> type
        val Monad =
            fun M -> sig struct
                sig map   : {A B:type} -> (A -> B) -> M A -> M B
                sig apply : {A B:type} -> M (A -> B) -> M A -> M B
                sig join  : {A:type} -> M (M A) -> M A
                sig bind  : {A B:type} -> (A -> M B) -> M A -> M B
            end

        ------------
        val Option : (type) -> type = fun A -> A | Unit

        sig some : {A:type} -> A -> Option A
        val some = fun a -> inl a

        sig none : {A:type} -> Option A
        val none = inr unit

        val EitherOption : Monad Option =
            rec(S:Monad Option).val struct
                val map   = fun {_ B} f ma -> case ma (fun a -> some (f a)) (fun _ -> none {B})
                val apply = fun {_ B} mf ma -> case mf (fun f -> #map S f ma) (fun _ -> none {B})
                val join  = fun {A} ma -> case ma (fun a -> a)  (fun _ -> none {A})
                val bind  = fun f ma -> #join S (#map S f ma)
            end

        val r : Option Unit = #map EitherOption (fun _ -> unit) (some 1)
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string))
    "Monad recursive record" expected (string_of_error result)

let compile_record_of_record () =
  let result =
    Pass.run
      {toy|
        sig int : type

        val m1 :
            sig struct
               sig m2 :
                  sig struct
                     sig m3 : int
               end
            end =
            val struct
               val m2 =
                  val struct
                     val m3 = 1
                  end
            end

        val m3 : int =
            let m = m1 in #m3 #m2 m
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string))
    "record of record" expected (string_of_error result)

let compile_control () =
  let result =
    Pass.run
      {toy|
        -- Preamble
        sig id : {a:type} -> a -> a
        sig compose : {a b c:type} -> (f: b -> c) -> (g: a -> b) -> a -> c
        --

        sig Functor : ((type) -> type) -> type
        val Functor = fun M ->
            sig struct
                sig map : {a b:type} -> (a -> b) -> M a -> M b
                -- Functor laws specification
                sig Law1 : {a:type} ->
                    let lhd = map (id {a}) in
                    let rhd = id {M a} in
                    equals lhd rhd
                sig Law2 : {a b c:type} -> (f: b -> c) -> (g: a -> b) ->
                    let lhd = map (compose f g) in
                    let rhd = compose (map f) (map g) in
                    equals lhd rhd
            end

        sig Applicative : ((type) -> type) -> type
        val Applicative = fun M ->
            sig struct
                sig pure  : {a:type} -> a -> M a
                sig apply : {a b:type} -> M (a -> b) -> M a -> M b
            end

        sig Monad : ((type) -> type) -> type
        val Monad = fun M ->
            sig struct
                sig return : {a:type} -> a -> M a
                sig bind   : {a b:type} -> M a -> (a -> M b) -> M b
            end
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string))
    "functor, applicative ..." expected (string_of_error result)

let compile_category () =
  let result =
    Pass.run
      {toy|
        -- Cf. https://arxiv.org/pdf/1401.7694.pdf

        sig Category : type
        val Category = sig struct
            sig Ob : type
            sig Hom : Ob -> Ob -> type
            sig compose : {a b c:Ob} -> Hom b c -> Hom a b -> Hom a c
            sig id : {x: Ob} -> Hom x x
            -- Laws
            sig Assoc : (a b c d:Ob) -> (f: Hom c d) -> (g: Hom b c) -> (h: Hom a b) -> equals (compose (compose f g) h) (compose f (compose g h))
            sig LeftId : (a b:Ob) -> (f: Hom a b) -> equals (compose id f) f
            sig RightId : (a b:Ob) -> (f: Hom a b) -> equals (compose f (id {a})) f -{ Here {a} should be specified! }-
            sig Truncated : (a b:Ob) -> (f g: Hom a b) -> (p q: equals f g) -> equals p q
        end
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string))
    "category ..." expected (string_of_error result)

let cases =
  let open Alcotest in
  ( "Record Compiler"
  , [
      test_case "Basic Record" `Quick compile_basic_record
    ; test_case "Basic Dependant Record type" `Quick
        compile_basic_dependant_record_type
    ; test_case "Basic Dependant Record and instance" `Quick
        compile_basic_dependant_record_type_and_instance
    ; test_case "Recursive Record" `Quick compile_recursive_record
    ; test_case "Monad Dependant Record" `Quick compile_monad_dependant_record
    ; test_case "Monad Recursive Record" `Quick compile_monad_recursive_record
    ; test_case "Record of Record" `Quick compile_record_of_record
    ; test_case "functor, applicative ..." `Quick compile_control
    ; test_case "category ..." `Quick compile_category
    ] )
