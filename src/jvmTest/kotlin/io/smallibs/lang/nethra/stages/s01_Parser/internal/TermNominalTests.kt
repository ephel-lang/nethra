package io.smallibs.lang.nethra.stages.s01_Parser.internal

import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe
import io.smallibs.lang.nethra.cst.Cst.prettyTerm
import io.smallibs.lang.nethra.stages.s01_Parser.internal.TermParser
import io.smallibs.parsec.io.Reader.Companion.string
import io.smallibs.parsec.parser.Common.get
import io.smallibs.parsec.parser.Flow.eos
import io.smallibs.parsec.parser.Flow.thenLeft

class TermNominalSpec : StringSpec({

    "[parser] Type" {
        (TermParser() thenLeft eos())(string("Type")).get()?.prettyTerm() shouldBe "Type"
    }

    "[parser] Int" {
        (TermParser() thenLeft eos())(string("Int")).get()?.prettyTerm() shouldBe "Int"
    }

    "[parser] Char" {
        (TermParser() thenLeft eos())(string("Char")).get()?.prettyTerm() shouldBe "Char"
    }

    "[parser] String" {
        (TermParser() thenLeft eos())(string("String")).get()?.prettyTerm() shouldBe "String"
    }

    "[parser] (Char)" {
        (TermParser() thenLeft eos())(string("(Char)")).get()?.prettyTerm() shouldBe "Char"
    }

    "[parser] Char -> Int" {
        (TermParser() thenLeft eos())(string("Char -> Int")).get()?.prettyTerm() shouldBe "Char -> Int"
    }

    "[parse] (e:Char) -> e" {
        (TermParser() thenLeft eos())(string("(e:Char) -> e")).get()?.prettyTerm() shouldBe "(e:Char) -> e"
    }

    "[parse] {e:Char} -> e" {
        (TermParser() thenLeft eos())(string("{e:Char} -> e")).get()?.prettyTerm() shouldBe "{e:Char} -> e"
    }

    "[parse] {e:Type} -> {v:e} -> v" {
        (TermParser() thenLeft eos())(string("{e:Type} -> {v:e} -> v")).get()
            ?.prettyTerm() shouldBe "{e:Type} -> {v:e} -> v"
    }

    "[parse] {e:Type} -> ({v:e} -> v)" {
        (TermParser() thenLeft eos())(string("{e:Type} -> ({v:e} -> v)")).get()
            ?.prettyTerm() shouldBe "{e:Type} -> {v:e} -> v"
    }

    "[parse] ({e:Type} -> e) -> v" {
        (TermParser() thenLeft eos())(string("({e:Type} -> e) -> v")).get()?.prettyTerm() shouldBe "{e:Type} -> e -> v"
    }

    "[parse] e f" {
        (TermParser() thenLeft eos())(string("e f")).get()?.prettyTerm() shouldBe "e f"
    }

    "[parse] e (f g)" {
        (TermParser() thenLeft eos())(string("e (f g)")).get()?.prettyTerm() shouldBe "e (f g)"
    }

    "[parse] e {f}" {
        (TermParser() thenLeft eos())(string("e {f}")).get()?.prettyTerm() shouldBe "e {f}"
    }

    "[parse] e * f" {
        (TermParser() thenLeft eos())(string("e * f")).get()?.prettyTerm() shouldBe "e * f"
    }

    "[parse] e * f -> g" {
        (TermParser() thenLeft eos())(string("e * f -> g")).get()?.prettyTerm() shouldBe "e * f -> g"
    }

    "[parse] (e * f) -> g" {
        (TermParser() thenLeft eos())(string("(e * f) -> g")).get()?.prettyTerm() shouldBe "e * f -> g"
    }

    "[parse] e f * g -> h" {
        (TermParser() thenLeft eos())(string("e f * g -> h")).get()?.prettyTerm() shouldBe "e f * g -> h"
    }

    "[parse] ((e f) * g) -> h" {
        (TermParser() thenLeft eos())(string("((e f) * g) -> h")).get()?.prettyTerm() shouldBe "e f * g -> h"
    }

    "[parse] e (f * g) -> h" {
        (TermParser() thenLeft eos())(string("e (f * g) -> h")).get()?.prettyTerm() shouldBe "e (f * g) -> h"
    }

    "[parse] (e) * (f -> g)" {
        (TermParser() thenLeft eos())(string("e * (f -> g)")).get()?.prettyTerm() shouldBe "e * (f -> g)"
    }

    "[parse] (e) * g" {
        (TermParser() thenLeft eos())(string("(e:t) * g")).get()?.prettyTerm() shouldBe "(e:t) * g"
    }

    "[parse] ?e" {
        (TermParser() thenLeft eos())(string("?e")).get()?.prettyTerm() shouldBe "?e"
    }

    "[parse] (e).e" {
        (TermParser() thenLeft eos())(string("(e).e")).get()?.prettyTerm() shouldBe "(e).e"
    }

    "[parse] {e}.e" {
        (TermParser() thenLeft eos())(string("{e}.e")).get()?.prettyTerm() shouldBe "{e}.e"
    }

    "[parse] {e}.(f e)" {
        (TermParser() thenLeft eos())(string("{e}.(f e)")).get()?.prettyTerm() shouldBe "{e}.(f e)"
    }

    "[parse] e | f" {
        (TermParser() thenLeft eos())(string("e | f")).get()?.prettyTerm() shouldBe "e | f"
    }

    "[parse] e | f -> g" {
        (TermParser() thenLeft eos())(string("e | f -> g")).get()?.prettyTerm() shouldBe "e | f -> g"
    }

    "[parse] (e | f) -> g" {
        (TermParser() thenLeft eos())(string("(e | f) -> g")).get()?.prettyTerm() shouldBe "e | f -> g"
    }

    "[parse] e f | g -> h" {
        (TermParser() thenLeft eos())(string("e f | g -> h")).get()?.prettyTerm() shouldBe "(e f) | g -> h"
    }

    "[parse] ((e f) | g) -> h" {
        (TermParser() thenLeft eos())(string("((e f) | g) -> h")).get()?.prettyTerm() shouldBe "(e f) | g -> h"
    }

    "[parse] e (f | g) -> h" {
        (TermParser() thenLeft eos())(string("e (f | g) -> h")).get()?.prettyTerm() shouldBe "e (f | g) -> h"
    }

    "[parse] (e) | (f -> g)" {
        (TermParser() thenLeft eos())(string("e | (f -> g)")).get()?.prettyTerm() shouldBe "e | (f -> g)"
    }

    "[parse] e , f | g" {
        (TermParser() thenLeft eos())(string("e , f | g")).get()?.prettyTerm() shouldBe "e , (f | g)"
    }

    "[parse] (e , f) | g" {
        (TermParser() thenLeft eos())(string("(e , f) | g")).get()?.prettyTerm() shouldBe "(e , f) | g"
    }

    "[parse] case True Int Char" {
        (TermParser() thenLeft eos())(string("case True Int Char")).get()?.prettyTerm() shouldBe "case True Int Char"
    }

    "[parse] inl 1" {
        (TermParser() thenLeft eos())(string("inl 1")).get()?.prettyTerm() shouldBe "inl 1"
    }

    "[parse] inr 1" {
        (TermParser() thenLeft eos())(string("inr 1")).get()?.prettyTerm() shouldBe "inr 1"
    }

    "[parse] {e:True|False} -> case e Int Char" {
        (TermParser() thenLeft eos())(string("{e:True|False} -> case e Int Char")).get()
            ?.prettyTerm() shouldBe "{e:True | False} -> case e Int Char"
    }

    "[parser] 42" {
        (TermParser() thenLeft eos())(string("42")).get()?.prettyTerm() shouldBe "42"
    }

    "[parser] +42" {
        (TermParser() thenLeft eos())(string("+42")).get()?.prettyTerm() shouldBe "42"
    }

    "[parser] -42" {
        (TermParser() thenLeft eos())(string("-42")).get()?.prettyTerm() shouldBe "-42"
    }

    "[parser] '4'" {
        (TermParser() thenLeft eos())(string("'4'")).get()?.prettyTerm() shouldBe "'4'"
    }

    "[parser] '\\''" {
        (TermParser() thenLeft eos())(string("'\\\''")).get()?.prettyTerm() shouldBe "'\\''"
    }

})

