package io.smallibs.lang.nethra.stages.s01_Parser.internal

import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe
import io.smallibs.lang.nethra.cst.Cst.prettyTerm
import io.smallibs.parsec.io.Reader.Companion.string
import io.smallibs.parsec.parser.Common.get
import io.smallibs.parsec.parser.Flow.eos
import io.smallibs.parsec.parser.Flow.thenLeft

class TermNominalSpec : StringSpec({

    "[parser] type" {
        (TermParser() thenLeft eos())(string("type")).get()?.prettyTerm() shouldBe "type0"
    }

    "[parser] int" {
        (TermParser() thenLeft eos())(string("int")).get()?.prettyTerm() shouldBe "int"
    }

    "[parser] char" {
        (TermParser() thenLeft eos())(string("char")).get()?.prettyTerm() shouldBe "char"
    }

    "[parser] string" {
        (TermParser() thenLeft eos())(string("string")).get()?.prettyTerm() shouldBe "string"
    }

    "[parser] data nil: (list x)" {
        (TermParser() thenLeft eos())(string("data nil:list x")).get()?.prettyTerm() shouldBe "data nil: (list x)"
    }

    "[parser] (char)" {
        (TermParser() thenLeft eos())(string("(char)")).get()?.prettyTerm() shouldBe "char"
    }

    "[parser] char -> int" {
        (TermParser() thenLeft eos())(string("char -> int")).get()?.prettyTerm() shouldBe "char -> int"
    }

    "[parse] (e:char) -> e" {
        (TermParser() thenLeft eos())(string("(e:char) -> e")).get()?.prettyTerm() shouldBe "(e:char) -> e"
    }

    "[parse] {e:char} -> e" {
        (TermParser() thenLeft eos())(string("{e:char} -> e")).get()?.prettyTerm() shouldBe "{e:char} -> e"
    }

    "[parse] {e:type} -> {v:e} -> v" {
        (TermParser() thenLeft eos())(string("{e:type} -> {v:e} -> v")).get()
            ?.prettyTerm() shouldBe "{e:type0} -> {v:e} -> v"
    }

    "[parse] {e:type} -> ({v:e} -> v)" {
        (TermParser() thenLeft eos())(string("{e:type} -> ({v:e} -> v)")).get()
            ?.prettyTerm() shouldBe "{e:type0} -> {v:e} -> v"
    }

    "[parse] ({e:type} -> e) -> v" {
        (TermParser() thenLeft eos())(string("({e:type} -> e) -> v")).get()?.prettyTerm() shouldBe "{e:type0} -> e -> v"
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

    "[parse] case True int char" {
        (TermParser() thenLeft eos())(string("case True int char")).get()?.prettyTerm() shouldBe "case True int char"
    }

    "[parse] inl 1" {
        (TermParser() thenLeft eos())(string("inl 1")).get()?.prettyTerm() shouldBe "inl 1"
    }

    "[parse] inr 1" {
        (TermParser() thenLeft eos())(string("inr 1")).get()?.prettyTerm() shouldBe "inr 1"
    }

    "[parse] rec(x).x " {
        (TermParser() thenLeft eos())(string("rec(x).x")).get()?.prettyTerm() shouldBe "rec(x).x"
    }

    "[parse] rec(x).({X:type} -> x X) " {
        (TermParser() thenLeft eos())(string("rec(x).({X:type} -> x X)")).get()?.prettyTerm() shouldBe "rec(x).({X:type0} -> x X)"
    }

    "[parse] {e:True|False} -> case e int char" {
        (TermParser() thenLeft eos())(string("{e:True|False} -> case e int char")).get()
            ?.prettyTerm() shouldBe "{e:True | False} -> case e int char"
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

