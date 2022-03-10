package io.smallibs.lang.nethra.stages.s01_Parser.internal

import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe
import io.smallibs.lang.nethra.cst.Cst.prettyPrint
import io.smallibs.parsec.io.Reader.Companion.string
import io.smallibs.parsec.parser.Common.get
import io.smallibs.parsec.parser.Flow.eos
import io.smallibs.parsec.parser.Flow.thenLeft

class TermNominalSpec : StringSpec({

    "[parser] type" {
        (TermParser() thenLeft eos())(string("type")).get()?.prettyPrint() shouldBe "type0"
    }

    "[parser] int" {
        (TermParser() thenLeft eos())(string("int")).get()?.prettyPrint() shouldBe "int"
    }

    "[parser] char" {
        (TermParser() thenLeft eos())(string("char")).get()?.prettyPrint() shouldBe "char"
    }

    "[parser] string" {
        (TermParser() thenLeft eos())(string("string")).get()?.prettyPrint() shouldBe "string"
    }

    "[parser] (char)" {
        (TermParser() thenLeft eos())(string("(char)")).get()?.prettyPrint() shouldBe "char"
    }

    "[parser] char -> int" {
        (TermParser() thenLeft eos())(string("char -> int")).get()?.prettyPrint() shouldBe "char -> int"
    }

    "[parse] (e:char) -> e" {
        (TermParser() thenLeft eos())(string("(e:char) -> e")).get()?.prettyPrint() shouldBe "(e:char) -> e"
    }

    "[parse] {e:char} -> e" {
        (TermParser() thenLeft eos())(string("{e:char} -> e")).get()?.prettyPrint() shouldBe "{e:char} -> e"
    }

    "[parse] {e:type} -> {v:e} -> v" {
        (TermParser() thenLeft eos())(string("{e:type} -> {v:e} -> v")).get()
            ?.prettyPrint() shouldBe "{e:type0} -> {v:e} -> v"
    }

    "[parse] {e:type} -> ({v:e} -> v)" {
        (TermParser() thenLeft eos())(string("{e:type} -> ({v:e} -> v)")).get()
            ?.prettyPrint() shouldBe "{e:type0} -> {v:e} -> v"
    }

    "[parse] ({e:type} -> e) -> v" {
        (TermParser() thenLeft eos())(string("({e:type} -> e) -> v")).get()?.prettyPrint() shouldBe "{e:type0} -> e -> v"
    }

    "[parse] e f" {
        (TermParser() thenLeft eos())(string("e f")).get()?.prettyPrint() shouldBe "e f"
    }

    "[parse] e (f g)" {
        (TermParser() thenLeft eos())(string("e (f g)")).get()?.prettyPrint() shouldBe "e (f g)"
    }

    "[parse] e {f}" {
        (TermParser() thenLeft eos())(string("e {f}")).get()?.prettyPrint() shouldBe "e {f}"
    }

    "[parse] e * f" {
        (TermParser() thenLeft eos())(string("e * f")).get()?.prettyPrint() shouldBe "e * f"
    }

    "[parse] e * f -> g" {
        (TermParser() thenLeft eos())(string("e * f -> g")).get()?.prettyPrint() shouldBe "e * f -> g"
    }

    "[parse] (e * f) -> g" {
        (TermParser() thenLeft eos())(string("(e * f) -> g")).get()?.prettyPrint() shouldBe "e * f -> g"
    }

    "[parse] e f * g -> h" {
        (TermParser() thenLeft eos())(string("e f * g -> h")).get()?.prettyPrint() shouldBe "e f * g -> h"
    }

    "[parse] ((e f) * g) -> h" {
        (TermParser() thenLeft eos())(string("((e f) * g) -> h")).get()?.prettyPrint() shouldBe "e f * g -> h"
    }

    "[parse] e (f * g) -> h" {
        (TermParser() thenLeft eos())(string("e (f * g) -> h")).get()?.prettyPrint() shouldBe "e (f * g) -> h"
    }

    "[parse] (e) * (f -> g)" {
        (TermParser() thenLeft eos())(string("e * (f -> g)")).get()?.prettyPrint() shouldBe "e * (f -> g)"
    }

    "[parse] (e) * g" {
        (TermParser() thenLeft eos())(string("(e:t) * g")).get()?.prettyPrint() shouldBe "(e:t) * g"
    }

    "[parse] ?e" {
        (TermParser() thenLeft eos())(string("?e")).get()?.prettyPrint() shouldBe "?e"
    }

    "[parse] (e).e" {
        (TermParser() thenLeft eos())(string("(e).e")).get()?.prettyPrint() shouldBe "(e).e"
    }

    "[parse] {e}.e" {
        (TermParser() thenLeft eos())(string("{e}.e")).get()?.prettyPrint() shouldBe "{e}.e"
    }

    "[parse] {e}.(f e)" {
        (TermParser() thenLeft eos())(string("{e}.(f e)")).get()?.prettyPrint() shouldBe "{e}.(f e)"
    }

    "[parse] e | f" {
        (TermParser() thenLeft eos())(string("e | f")).get()?.prettyPrint() shouldBe "e | f"
    }

    "[parse] e | f -> g" {
        (TermParser() thenLeft eos())(string("e | f -> g")).get()?.prettyPrint() shouldBe "e | f -> g"
    }

    "[parse] (e | f) -> g" {
        (TermParser() thenLeft eos())(string("(e | f) -> g")).get()?.prettyPrint() shouldBe "e | f -> g"
    }

    "[parse] e f | g -> h" {
        (TermParser() thenLeft eos())(string("e f | g -> h")).get()?.prettyPrint() shouldBe "(e f) | g -> h"
    }

    "[parse] ((e f) | g) -> h" {
        (TermParser() thenLeft eos())(string("((e f) | g) -> h")).get()?.prettyPrint() shouldBe "(e f) | g -> h"
    }

    "[parse] e (f | g) -> h" {
        (TermParser() thenLeft eos())(string("e (f | g) -> h")).get()?.prettyPrint() shouldBe "e (f | g) -> h"
    }

    "[parse] (e) | (f -> g)" {
        (TermParser() thenLeft eos())(string("e | (f -> g)")).get()?.prettyPrint() shouldBe "e | (f -> g)"
    }

    "[parse] e , f | g" {
        (TermParser() thenLeft eos())(string("e , f | g")).get()?.prettyPrint() shouldBe "e , (f | g)"
    }

    "[parse] (e , f) | g" {
        (TermParser() thenLeft eos())(string("(e , f) | g")).get()?.prettyPrint() shouldBe "(e , f) | g"
    }

    "[parse] case True int char" {
        (TermParser() thenLeft eos())(string("case True int char")).get()?.prettyPrint() shouldBe "case True int char"
    }

    "[parse] inl 1" {
        (TermParser() thenLeft eos())(string("inl 1")).get()?.prettyPrint() shouldBe "inl 1"
    }

    "[parse] inr 1" {
        (TermParser() thenLeft eos())(string("inr 1")).get()?.prettyPrint() shouldBe "inr 1"
    }

    "[parse] rec(x).x " {
        (TermParser() thenLeft eos())(string("rec(x).x")).get()?.prettyPrint() shouldBe "rec(x).x"
    }

    "[parse] rec(x).({X:type} -> x X) " {
        (TermParser() thenLeft eos())(string("rec(x).({X:type} -> x X)")).get()?.prettyPrint() shouldBe "rec(x).({X:type0} -> x X)"
    }

    "[parse] {e:True|False} -> case e int char" {
        (TermParser() thenLeft eos())(string("{e:True|False} -> case e int char")).get()
            ?.prettyPrint() shouldBe "{e:True | False} -> case e int char"
    }

    "[parser] 42" {
        (TermParser() thenLeft eos())(string("42")).get()?.prettyPrint() shouldBe "42"
    }

    "[parser] +42" {
        (TermParser() thenLeft eos())(string("+42")).get()?.prettyPrint() shouldBe "42"
    }

    "[parser] -42" {
        (TermParser() thenLeft eos())(string("-42")).get()?.prettyPrint() shouldBe "-42"
    }

    "[parser] '4'" {
        (TermParser() thenLeft eos())(string("'4'")).get()?.prettyPrint() shouldBe "'4'"
    }

    "[parser] '\\''" {
        (TermParser() thenLeft eos())(string("'\\\''")).get()?.prettyPrint() shouldBe "'\\''"
    }

})

