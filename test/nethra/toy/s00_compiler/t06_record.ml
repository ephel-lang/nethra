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

let compile_basic_dependant_record () =
  let result =
    Pass.run
      {toy|
        -----------
        sig int : type
        sig add : int -> int -> int
        -----------

        sig Monoid : type
        val Monoid =
            sig struct
                sig self    : type
                sig neutral : self
                sig combine : self -> self -> self
                -- sig law1 : self -> self
            end

        -----------

        sig MonoidInt : Monoid
        val MonoidInt =
            val struct
                val self    = int
                val neutral = 0
                val combine = add
                -- val law1 = combine neutral
            end
        -{
        sig test : int
        val test =
            let n = #neutral MonoidInt in
            (#combine MonoidInt 1 n)
        }-
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string))
    "Basic dependant record" expected (string_of_error result)

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
            (t).sig struct
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
                sig mv : (self -> (int * int) -> self)
            end

        val Point : int -> int -> point =
            (x).(y).fold val struct
                val x  = x
                val y  = y
                val mv = (self).(x_and_y).
                    let nx = add (fst x_and_y) (#x unfold self) in
                    let ny = add (snd x_and_y) (#y unfold self) in
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
            (M).sig struct
                sig map   : {A B:type} -> (A -> B) -> M A -> M B
                sig apply : {A B:type} -> M (A -> B) -> M A -> M B
                sig join  : {A:type} -> M (M A) -> M A
                sig bind  : {A B:type} -> (A -> M B) -> M A -> M B
            end

        ------------
        val Option : (type) -> type = (A).(A | Unit)

        sig some : {A:type} -> A -> Option A
        val some = (a).inl a

        sig none : {A:type} -> Option A
        val none = inr unit

        sig EitherOption : Monad Option
        -{
        val EitherOption =
            val struct
                val map   = {_ B}.(f ma).(case ma (a).(some (f a)) (_).(none {B}))
                val apply = {_ B}.(mf ma).(case mf (f).(map f ma) (_).(none {B}))
                val join  = {A}.(ma).(case ma (a).a (_).(none {A}))
                val bind  = (f ma).(join (map f ma))
            end

        val r : Option Unit = #map EitherOption (_).unit (some 1)
        }-
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
            (M).sig struct
                sig map   : {A B:type} -> (A -> B) -> M A -> M B
                sig apply : {A B:type} -> M (A -> B) -> M A -> M B
                sig join  : {A:type} -> M (M A) -> M A
                sig bind  : {A B:type} -> (A -> M B) -> M A -> M B
            end

        ------------
        val Option : (type) -> type = (A).(A | Unit)

        sig some : {A:type} -> A -> Option A
        val some = (a).inl a

        sig none : {A:type} -> Option A
        val none = inr unit

        val EitherOption : Monad Option =
            rec(S:Monad Option).val struct
                val map   = {_ B}.(f ma).(case ma (a).(some (f a)) (_).(none {B}))
                val apply = {_ B}.(mf ma).(case mf (f).(#map S f ma) (_).(none {B}))
                val join  = {A}.(ma).(case ma (a).a (_).(none {A}))
                val bind  = (f ma).(#join S (#map S f ma))
            end

        val r : Option Unit = #map EitherOption (_).unit (some 1)
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string))
    "Monad recursive record" expected (string_of_error result)

let cases =
  let open Alcotest in
  ( "Record Compiler"
  , [
      test_case "Basic Record" `Quick compile_basic_record
    ; test_case "Basic Dependant Record type" `Quick
        compile_basic_dependant_record_type
    ; test_case "Basic Dependant Record" `Quick compile_basic_dependant_record
    ; test_case "Recursive Record" `Quick compile_recursive_record
    ; test_case "Monad Dependant Record" `Quick compile_monad_dependant_record
    ; test_case "Monad Recursive Record" `Quick compile_monad_recursive_record
    ] )