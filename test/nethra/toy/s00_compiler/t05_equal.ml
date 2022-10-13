open Common
open Nethra.Toy.Compiler

open Preface_stdlib.Result.Functor (struct
  type t = Nethra.Syntax.Source.Region.t Pass.error
end)

let compile_equal () =
  let result =
    Pass.run
      {toy|
      -{
        Leibniz equality
      }-

      sig equal : {A:type} -> (a:A) -> (b:A) -> type
      val equal = {A}.(a).(b).((P : A -> type) -> P a -> P b)

      sig reflexive : {A:type} -> {a:A} -> equal a a
      val reflexive = (P).(Pa).Pa

      sig transitive : {A:type} -> {a:A} -> {b:A} -> {c:A} -> equal a b -> equal b c -> equal a c
      val transitive = (eq_a_b).(eq_b_c).(P).(Pa).(eq_b_c P (eq_a_b P Pa))

      sig symmetric : {A:type} -> {a:A} -> {b:A} -> equal a b -> equal b a
      val symmetric = {A}.{a}.(eq_a_b).(P).
            let Qa = reflexive P in
            let Qb = eq_a_b (c).(P c -> P a) Qa in
            Qb
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string))
    "Leibniz equality" expected (string_of_error result)

let cases =
  let open Alcotest in
  ("Equal Compiler", [ test_case "Leibniz equality" `Quick compile_equal ])
