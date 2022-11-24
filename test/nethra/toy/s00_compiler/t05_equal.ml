open Common
open Nethra.Toy.Compiler

open Preface_stdlib.Result.Functor (struct
  type t = Nethra.Syntax.Source.Region.t Pass.error
end)

let compile_propositional_equal () =
  let result =
    Pass.run
      {toy|
      -{
        Propositional equality
      }-

      sig reflexive : {A:type} -> {a:A} -> equals a a
      val reflexive = refl

      sig symmetric : {A:type} -> {a b:A} -> equals a b -> equals b a
      val symmetric = (a_eq_b).subst refl by a_eq_b

      sig transitivity : {A:type} -> {a b c :A} -> equals a b -> equals b c -> equals a c
      val transitivity = (a_eq_b b_eq_c).subst (subst refl by a_eq_b) by b_eq_c
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string))
    "Propositional equality" expected (string_of_error result)

let compile_leibniz_equal () =
  let result =
    Pass.run
      {toy|
      -{
        Leibniz equality
      }-

      sig equal : {A:type} -> (a b:A) -> type
      val equal = {A}.(a b).((P : A -> type) -> P a -> P b)

      sig reflexive : {A:type} -> {a:A} -> equal a a
      val reflexive = (P Pa).Pa

      sig transitive : {A:type} -> {a b c:A} -> equal a b -> equal b c -> equal a c
      val transitive = (a_eq_b b_eq_c).(P Pa).(b_eq_c P (a_eq_b P Pa))

      sig symmetric : {A:type} -> {a b:A} -> equal a b -> equal b a
      val symmetric = {A a}.(a_eq_b).(P).
            let Qa = reflexive P in
            let Qb = a_eq_b (c).(P c -> P a) Qa in
            Qb
      |toy}
    <&> fun (_, l) -> check l
  and expected = Result.Ok true in
  Alcotest.(check (result bool string))
    "Leibniz equality" expected (string_of_error result)

let cases =
  let open Alcotest in
  ( "Equal Compiler"
  , [
      test_case "Propositional equality" `Quick compile_propositional_equal
    ; test_case "Leibniz equality" `Quick compile_leibniz_equal
    ] )