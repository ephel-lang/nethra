open Common
open Nethra.Toy.Compiler

let compile_equal () =
  let open Preface_stdlib.Result.Functor (struct
    type t = string
  end) in
  let result =
    Pass.run
      {toy|
      -{
        Leibniz equality
      }-

      sig equal : {A:type} -> (a:A) -> (b:A) -> type
      def equal = {A}.(a).(b).((P : A -> type) -> P a -> P b)

      sig reflexive : {A:type} -> {a:A} -> equal a a
      def reflexive = (P).(Pa).Pa

      sig transitive : {A:type} -> {a:A} -> {b:A} -> {c:A} -> equal a b -> equal b c -> equal a c
      def transitive = (eq_a_b).(eq_b_c).(P).(Pa).(eq_b_c P (eq_a_b P Pa))

      sig symmetric : {A:type} -> {a:A} -> {b:A} -> equal a b -> equal b a
      def symmetric = {A}.{a}.{b}.(eq_a_b).(P).(eq_a_b (c).(P c -> P a) (reflexive P))
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string)) "basic equal" expected result

let cases =
  let open Alcotest in
  ("Equal Compiler", [ test_case "basic equal" `Quick compile_equal ])