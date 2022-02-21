package io.smallibs.lang.nethra.parser

import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe
import io.smallibs.lang.nethra.parser.Cst.pretty
import io.smallibs.parsec.io.Reader
import io.smallibs.parsec.parser.Common.get
import io.smallibs.parsec.parser.Flow.eos
import io.smallibs.parsec.parser.Flow.thenLeft

class NominalTypeSpec : StringSpec({

    "[parser] Type" {
        (Term() thenLeft eos())(Reader.string("Type")).get()?.pretty() shouldBe "Type"
    }

    "[parser] Int" {
        (Term() thenLeft eos())(Reader.string("Int")).get()?.pretty() shouldBe "Int"
    }

    "[parser] Char" {
        (Term() thenLeft eos())(Reader.string("Char")).get()?.pretty() shouldBe "Char"
    }

    "[parser] (Char)" {
        (Term() thenLeft eos())(Reader.string("(Char)")).get()?.pretty() shouldBe "Char"
    }

    "[parser] Char -> Int" {
        (Term() thenLeft eos())(Reader.string("Char -> Int")).get()?.pretty() shouldBe "Char -> Int"
    }

    "[parse] (e:Char) -> e" {
        (Term() thenLeft eos())(Reader.string("(e:Char) -> e")).get()?.pretty() shouldBe "(e:Char) -> e"
    }

    "[parse] {e:Char} -> e" {
        (Term() thenLeft eos())(Reader.string("{e:Char} -> e")).get()?.pretty() shouldBe "{e:Char} -> e"
    }

    "[parse] {e:Type} -> {v:e} -> v" {
        (Term() thenLeft eos())(Reader.string("{e:Type} -> {v:e} -> v")).get()?.pretty() shouldBe "{e:Type} -> {v:e} -> v"
    }

    "[parse] {e:Type} -> ({v:e} -> v)" {
        (Term() thenLeft eos())(Reader.string("{e:Type} -> ({v:e} -> v)")).get()?.pretty() shouldBe "{e:Type} -> {v:e} -> v"
    }

    "[parse] ({e:Type} -> e) -> v" {
        (Term() thenLeft eos())(Reader.string("({e:Type} -> e) -> v")).get()?.pretty() shouldBe "{e:Type} -> e -> v"
    }

    "[parse] e f" {
        (Term() thenLeft eos())(Reader.string("e f")).get()?.pretty() shouldBe "e f"
    }

    "[parse] e (f g)" {
        (Term() thenLeft eos())(Reader.string("e (f g)")).get()?.pretty() shouldBe "e (f g)"
    }

    "[parse] e {f}" {
        (Term() thenLeft eos())(Reader.string("e {f}")).get()?.pretty() shouldBe "e {f}"
    }

    "[parse] e * f" {
        (Term() thenLeft eos())(Reader.string("e * f")).get()?.pretty() shouldBe "e * f"
    }

    "[parse] e * f -> g" {
        (Term() thenLeft eos())(Reader.string("e * f -> g")).get()?.pretty() shouldBe "e * f -> g"
    }

    "[parse] (e * f) -> g" {
        (Term() thenLeft eos())(Reader.string("(e * f) -> g")).get()?.pretty() shouldBe "e * f -> g"
    }

    "[parse] e f * g -> h" {
        (Term() thenLeft eos())(Reader.string("e f * g -> h")).get()?.pretty() shouldBe "e f * g -> h"
    }

    "[parse] ((e f) * g) -> h" {
        (Term() thenLeft eos())(Reader.string("((e f) * g) -> h")).get()?.pretty() shouldBe "e f * g -> h"
    }

    "[parse] e (f * g) -> h" {
        (Term() thenLeft eos())(Reader.string("e (f * g) -> h")).get()?.pretty() shouldBe "e (f * g) -> h"
    }

    "[parse] (e) * (f -> g)" {
        (Term() thenLeft eos())(Reader.string("e * (f -> g)")).get()?.pretty() shouldBe "e * (f -> g)"
    }

    "[parse] (e) * g" {
        (Term() thenLeft eos())(Reader.string("(e:t) * g")).get()?.pretty() shouldBe "(e:t) * g"
    }

    "[parse] ?e" {
        (Term() thenLeft eos())(Reader.string("?e")).get()?.pretty() shouldBe "?e"
    }

    "[parse] (e).e" {
        (Term() thenLeft eos())(Reader.string("(e).e")).get()?.pretty() shouldBe "(e).e"
    }

    "[parse] {e}.e" {
        (Term() thenLeft eos())(Reader.string("{e}.e")).get()?.pretty() shouldBe "{e}.e"
    }

    "[parse] {e}.(f e)" {
        (Term() thenLeft eos())(Reader.string("{e}.(f e)")).get()?.pretty() shouldBe "{e}.(f e)"
    }
})

