package io.smallibs.lang.nethra.stages.s01_Parser

import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe
import io.smallibs.lang.nethra.cst.Cst.prettyTerm
import io.smallibs.parsec.io.Reader.Companion.string
import io.smallibs.parsec.parser.Common.get
import io.smallibs.parsec.parser.Flow.eos
import io.smallibs.parsec.parser.Flow.thenLeft

class TermNominalSpec : StringSpec({

    "[parser] Type" {
        (Term() thenLeft eos())(string("Type")).get()?.prettyTerm() shouldBe "Type"
    }

    "[parser] Int" {
        (Term() thenLeft eos())(string("Int")).get()?.prettyTerm() shouldBe "Int"
    }

    "[parser] Char" {
        (Term() thenLeft eos())(string("Char")).get()?.prettyTerm() shouldBe "Char"
    }

    "[parser] String" {
        (Term() thenLeft eos())(string("String")).get()?.prettyTerm() shouldBe "String"
    }

    "[parser] (Char)" {
        (Term() thenLeft eos())(string("(Char)")).get()?.prettyTerm() shouldBe "Char"
    }

    "[parser] Char -> Int" {
        (Term() thenLeft eos())(string("Char -> Int")).get()?.prettyTerm() shouldBe "Char -> Int"
    }

    "[parse] (e:Char) -> e" {
        (Term() thenLeft eos())(string("(e:Char) -> e")).get()?.prettyTerm() shouldBe "(e:Char) -> e"
    }

    "[parse] {e:Char} -> e" {
        (Term() thenLeft eos())(string("{e:Char} -> e")).get()?.prettyTerm() shouldBe "{e:Char} -> e"
    }

    "[parse] {e:Type} -> {v:e} -> v" {
        (Term() thenLeft eos())(string("{e:Type} -> {v:e} -> v")).get()?.prettyTerm() shouldBe "{e:Type} -> {v:e} -> v"
    }

    "[parse] {e:Type} -> ({v:e} -> v)" {
        (Term() thenLeft eos())(string("{e:Type} -> ({v:e} -> v)")).get()?.prettyTerm() shouldBe "{e:Type} -> {v:e} -> v"
    }

    "[parse] ({e:Type} -> e) -> v" {
        (Term() thenLeft eos())(string("({e:Type} -> e) -> v")).get()?.prettyTerm() shouldBe "{e:Type} -> e -> v"
    }

    "[parse] e f" {
        (Term() thenLeft eos())(string("e f")).get()?.prettyTerm() shouldBe "e f"
    }

    "[parse] e (f g)" {
        (Term() thenLeft eos())(string("e (f g)")).get()?.prettyTerm() shouldBe "e (f g)"
    }

    "[parse] e {f}" {
        (Term() thenLeft eos())(string("e {f}")).get()?.prettyTerm() shouldBe "e {f}"
    }

    "[parse] e * f" {
        (Term() thenLeft eos())(string("e * f")).get()?.prettyTerm() shouldBe "e * f"
    }

    "[parse] e * f -> g" {
        (Term() thenLeft eos())(string("e * f -> g")).get()?.prettyTerm() shouldBe "e * f -> g"
    }

    "[parse] (e * f) -> g" {
        (Term() thenLeft eos())(string("(e * f) -> g")).get()?.prettyTerm() shouldBe "e * f -> g"
    }

    "[parse] e f * g -> h" {
        (Term() thenLeft eos())(string("e f * g -> h")).get()?.prettyTerm() shouldBe "e f * g -> h"
    }

    "[parse] ((e f) * g) -> h" {
        (Term() thenLeft eos())(string("((e f) * g) -> h")).get()?.prettyTerm() shouldBe "e f * g -> h"
    }

    "[parse] e (f * g) -> h" {
        (Term() thenLeft eos())(string("e (f * g) -> h")).get()?.prettyTerm() shouldBe "e (f * g) -> h"
    }

    "[parse] (e) * (f -> g)" {
        (Term() thenLeft eos())(string("e * (f -> g)")).get()?.prettyTerm() shouldBe "e * (f -> g)"
    }

    "[parse] (e) * g" {
        (Term() thenLeft eos())(string("(e:t) * g")).get()?.prettyTerm() shouldBe "(e:t) * g"
    }

    "[parse] ?e" {
        (Term() thenLeft eos())(string("?e")).get()?.prettyTerm() shouldBe "?e"
    }

    "[parse] (e).e" {
        (Term() thenLeft eos())(string("(e).e")).get()?.prettyTerm() shouldBe "(e).e"
    }

    "[parse] {e}.e" {
        (Term() thenLeft eos())(string("{e}.e")).get()?.prettyTerm() shouldBe "{e}.e"
    }

    "[parse] {e}.(f e)" {
        (Term() thenLeft eos())(string("{e}.(f e)")).get()?.prettyTerm() shouldBe "{e}.(f e)"
    }

    "[parse] e | f" {
        (Term() thenLeft eos())(string("e | f")).get()?.prettyTerm() shouldBe "e | f"
    }

    "[parse] e | f -> g" {
        (Term() thenLeft eos())(string("e | f -> g")).get()?.prettyTerm() shouldBe "e | f -> g"
    }

    "[parse] (e | f) -> g" {
        (Term() thenLeft eos())(string("(e | f) -> g")).get()?.prettyTerm() shouldBe "e | f -> g"
    }

    "[parse] e f | g -> h" {
        (Term() thenLeft eos())(string("e f | g -> h")).get()?.prettyTerm() shouldBe "e f | g -> h"
    }

    "[parse] ((e f) | g) -> h" {
        (Term() thenLeft eos())(string("((e f) | g) -> h")).get()?.prettyTerm() shouldBe "e f | g -> h"
    }

    "[parse] e (f | g) -> h" {
        (Term() thenLeft eos())(string("e (f | g) -> h")).get()?.prettyTerm() shouldBe "e (f | g) -> h"
    }

    "[parse] (e) | (f -> g)" {
        (Term() thenLeft eos())(string("e | (f -> g)")).get()?.prettyTerm() shouldBe "e | (f -> g)"
    }

    "[parse] case True Int Char" {
        (Term() thenLeft eos())(string("case True Int Char")).get()?.prettyTerm() shouldBe "case True Int Char"
    }

    "[parse] {e:True|False} -> case e Int Char" {
        (Term() thenLeft eos())(string("{e:True|False} -> case e Int Char")).get()
            ?.prettyTerm() shouldBe "{e:True | False} -> case e Int Char"
    }

    "[parser] 42" {
        (Term() thenLeft eos())(string("42")).get()?.prettyTerm() shouldBe "42"
    }

    "[parser] +42" {
        (Term() thenLeft eos())(string("+42")).get()?.prettyTerm() shouldBe "42"
    }

    "[parser] -42" {
        (Term() thenLeft eos())(string("-42")).get()?.prettyTerm() shouldBe "-42"
    }

    "[parser] '4'" {
        (Term() thenLeft eos())(string("'4'")).get()?.prettyTerm() shouldBe "'4'"
    }

    "[parser] '\\''" {
        (Term() thenLeft eos())(string("'\\\''")).get()?.prettyTerm() shouldBe "'\\''"
    }

})

