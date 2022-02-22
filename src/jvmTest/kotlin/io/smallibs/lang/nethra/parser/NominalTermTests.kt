package io.smallibs.lang.nethra.parser

import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe
import io.smallibs.lang.nethra.cst.Cst.pretty
import io.smallibs.parsec.io.Reader.Companion.string
import io.smallibs.parsec.parser.Common.get
import io.smallibs.parsec.parser.Flow.eos
import io.smallibs.parsec.parser.Flow.thenLeft

class NominalTypeSpec : StringSpec({

    "[parser] Type" {
        (Term() thenLeft eos())(string("Type")).get()?.pretty() shouldBe "Type"
    }

    "[parser] Int" {
        (Term() thenLeft eos())(string("Int")).get()?.pretty() shouldBe "Int"
    }

    "[parser] Char" {
        (Term() thenLeft eos())(string("Char")).get()?.pretty() shouldBe "Char"
    }

    "[parser] String" {
        (Term() thenLeft eos())(string("String")).get()?.pretty() shouldBe "String"
    }

    "[parser] (Char)" {
        (Term() thenLeft eos())(string("(Char)")).get()?.pretty() shouldBe "Char"
    }

    "[parser] Char -> Int" {
        (Term() thenLeft eos())(string("Char -> Int")).get()?.pretty() shouldBe "Char -> Int"
    }

    "[parse] (e:Char) -> e" {
        (Term() thenLeft eos())(string("(e:Char) -> e")).get()?.pretty() shouldBe "(e:Char) -> e"
    }

    "[parse] {e:Char} -> e" {
        (Term() thenLeft eos())(string("{e:Char} -> e")).get()?.pretty() shouldBe "{e:Char} -> e"
    }

    "[parse] {e:Type} -> {v:e} -> v" {
        (Term() thenLeft eos())(string("{e:Type} -> {v:e} -> v")).get()?.pretty() shouldBe "{e:Type} -> {v:e} -> v"
    }

    "[parse] {e:Type} -> ({v:e} -> v)" {
        (Term() thenLeft eos())(string("{e:Type} -> ({v:e} -> v)")).get()?.pretty() shouldBe "{e:Type} -> {v:e} -> v"
    }

    "[parse] ({e:Type} -> e) -> v" {
        (Term() thenLeft eos())(string("({e:Type} -> e) -> v")).get()?.pretty() shouldBe "{e:Type} -> e -> v"
    }

    "[parse] e f" {
        (Term() thenLeft eos())(string("e f")).get()?.pretty() shouldBe "e f"
    }

    "[parse] e (f g)" {
        (Term() thenLeft eos())(string("e (f g)")).get()?.pretty() shouldBe "e (f g)"
    }

    "[parse] e {f}" {
        (Term() thenLeft eos())(string("e {f}")).get()?.pretty() shouldBe "e {f}"
    }

    "[parse] e * f" {
        (Term() thenLeft eos())(string("e * f")).get()?.pretty() shouldBe "e * f"
    }

    "[parse] e * f -> g" {
        (Term() thenLeft eos())(string("e * f -> g")).get()?.pretty() shouldBe "e * f -> g"
    }

    "[parse] (e * f) -> g" {
        (Term() thenLeft eos())(string("(e * f) -> g")).get()?.pretty() shouldBe "e * f -> g"
    }

    "[parse] e f * g -> h" {
        (Term() thenLeft eos())(string("e f * g -> h")).get()?.pretty() shouldBe "e f * g -> h"
    }

    "[parse] ((e f) * g) -> h" {
        (Term() thenLeft eos())(string("((e f) * g) -> h")).get()?.pretty() shouldBe "e f * g -> h"
    }

    "[parse] e (f * g) -> h" {
        (Term() thenLeft eos())(string("e (f * g) -> h")).get()?.pretty() shouldBe "e (f * g) -> h"
    }

    "[parse] (e) * (f -> g)" {
        (Term() thenLeft eos())(string("e * (f -> g)")).get()?.pretty() shouldBe "e * (f -> g)"
    }

    "[parse] (e) * g" {
        (Term() thenLeft eos())(string("(e:t) * g")).get()?.pretty() shouldBe "(e:t) * g"
    }

    "[parse] ?e" {
        (Term() thenLeft eos())(string("?e")).get()?.pretty() shouldBe "?e"
    }

    "[parse] (e).e" {
        (Term() thenLeft eos())(string("(e).e")).get()?.pretty() shouldBe "(e).e"
    }

    "[parse] {e}.e" {
        (Term() thenLeft eos())(string("{e}.e")).get()?.pretty() shouldBe "{e}.e"
    }

    "[parse] {e}.(f e)" {
        (Term() thenLeft eos())(string("{e}.(f e)")).get()?.pretty() shouldBe "{e}.(f e)"
    }

    "[parse] e | f" {
        (Term() thenLeft eos())(string("e | f")).get()?.pretty() shouldBe "e | f"
    }

    "[parse] e | f -> g" {
        (Term() thenLeft eos())(string("e | f -> g")).get()?.pretty() shouldBe "e | f -> g"
    }

    "[parse] (e | f) -> g" {
        (Term() thenLeft eos())(string("(e | f) -> g")).get()?.pretty() shouldBe "e | f -> g"
    }

    "[parse] e f | g -> h" {
        (Term() thenLeft eos())(string("e f | g -> h")).get()?.pretty() shouldBe "e f | g -> h"
    }

    "[parse] ((e f) | g) -> h" {
        (Term() thenLeft eos())(string("((e f) | g) -> h")).get()?.pretty() shouldBe "e f | g -> h"
    }

    "[parse] e (f | g) -> h" {
        (Term() thenLeft eos())(string("e (f | g) -> h")).get()?.pretty() shouldBe "e (f | g) -> h"
    }

    "[parse] (e) | (f -> g)" {
        (Term() thenLeft eos())(string("e | (f -> g)")).get()?.pretty() shouldBe "e | (f -> g)"
    }

    "[parse] case True Int Char" {
        (Term() thenLeft eos())(string("case True Int Char")).get()?.pretty() shouldBe "case True Int Char"
    }

    "[parse] {e:True|False} -> case e Int Char" {
        (Term() thenLeft eos())(string("{e:True|False} -> case e Int Char")).get()
            ?.pretty() shouldBe "{e:True | False} -> case e Int Char"
    }

    "[parser] 42" {
        (Term() thenLeft eos())(string("42")).get()?.pretty() shouldBe "42"
    }

    "[parser] +42" {
        (Term() thenLeft eos())(string("+42")).get()?.pretty() shouldBe "42"
    }

    "[parser] -42" {
        (Term() thenLeft eos())(string("-42")).get()?.pretty() shouldBe "-42"
    }

    "[parser] '4'" {
        (Term() thenLeft eos())(string("'4'")).get()?.pretty() shouldBe "'4'"
    }

    "[parser] '\\''" {
        (Term() thenLeft eos())(string("'\\\''")).get()?.pretty() shouldBe "'\\''"
    }

})

