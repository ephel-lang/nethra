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

      sig reflexive :
            {A:type} -> {a:A}
            -------------
            -> equals a a

      val reflexive =
            refl

      sig symmetric :
            {A:type} -> {a b:A}
            -> equals a b
            -------------
            -> equals b a

      val symmetric = fun a=b ->
            subst refl by a=b

      sig transitivity :
            {A:type} -> {a b c :A}
            -> equals a b
            -> equals b c
            -------------
            -> equals a c

      val transitivity = fun a=b b=c ->
            subst (subst refl by a=b) by b=c

      -----------------------------------------------------------

      sig congruent :
            {A B:type} -> (f:A -> B) -> {a b:A}
            -> equals a b
            ---------------------
            -> equals (f a) (f b)

      val congruent = fun f a=b ->
            subst refl by (a=b)

      sig congruent_2 :
            {A B C:type} -> (f:A -> B -> C) -> {a b:A} -> {c d:B}
            -> equals a b
            -> equals c d
            -------------------------
            -> equals (f a c) (f b d)

      val congruent_2 = fun f a=b c=d ->
            subst (subst refl by a=b) by c=d

      sig congruent_app : {A B:type} -> (f g:A -> B)
            -> equals f g
            ------------------------------
            -> {a:A} -> equals (f a) (g a)

      val congruent_app = fun f g f=g ->
            subst refl by (f=g)

      sig substitution : {A:type} -> {x y:A} -> (P:A -> type)
            -> equals x y
            -------------
            -> P x -> P y

      val substitution = fun P x=y px ->
            subst px by x=y
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
      val equal = fun {A} a b -> (P : A -> type) -> P a -> P b

      sig reflexive :
                {A:type} -> {a:A}
                ------------
                -> equal a a

      val reflexive = fun P Pa -> Pa

      sig transitive :
                {A:type} -> {a b c:A}
                -> equal a b
                -> equal b c
                ------------
                -> equal a c

      val transitive = fun a=b b=c P Pa -> b=c P (a=b P Pa)

      sig symmetric :
                {A:type} -> {a b:A}
                -> equal a b
                ------------
                -> equal b a

      val symmetric = fun {A a b} a=b P ->
            let Q = A -> type in
            let Q = fun c -> P c -> P a in
            let Qa : Q a = reflexive P in
            let Qb : Q b = a=b Q Qa in
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
