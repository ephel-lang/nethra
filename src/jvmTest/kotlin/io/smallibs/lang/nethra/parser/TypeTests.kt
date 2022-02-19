package io.smallibs.lang.nethra.parser

import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe
import io.smallibs.lang.nethra.parser.Cst.pretty
import io.smallibs.parsec.io.Reader
import io.smallibs.parsec.parser.Common.get
import io.smallibs.parsec.parser.Common.isSuccess
import io.smallibs.parsec.parser.Flow.eos
import io.smallibs.parsec.parser.Flow.thenLeft

class TypeSpec : StringSpec({

    "[parser] Int" {
        (Type() thenLeft eos())(Reader.string("Int")).get()?.pretty() shouldBe "Int"
    }

    "[parser] Char" {
        (Type() thenLeft eos())(Reader.string("Char")).get()?.pretty() shouldBe "Char"
    }

    "[parser] (Char)" {
        (Type() thenLeft eos())(Reader.string("(Char)")).get()?.pretty() shouldBe "Char"
    }

    "[parser] Char -> Int" {
        (Type() thenLeft eos())(Reader.string("Char -> Int")).get()?.pretty() shouldBe "Char -> Int"
    }

    "[parse] (e:Char) -> e" {
        (Type() thenLeft eos())(Reader.string("(e:Char) -> e")).get()?.pretty() shouldBe "(e:Char) -> e"
    }

    "[parse] {e:Char} -> e" {
        (Type() thenLeft eos())(Reader.string("{e:Char} -> e")).get()?.pretty() shouldBe "{e:Char} -> e"
    }

    "[parse] {e:Type} -> {v:e} -> v" {
        (Type() thenLeft eos())(Reader.string("{e:Type} -> {v:e} -> v")).get()?.pretty() shouldBe "{e:*} -> {v:e} -> v"
    }

    "[parse] {e:Type} -> ({v:e} -> v)" {
        (Type() thenLeft eos())(Reader.string("{e:Type} -> ({v:e} -> v)")).get()?.pretty() shouldBe "{e:*} -> {v:e} -> v"
    }

    "[parse] ({e:Type} -> e) -> v" {
        (Type() thenLeft eos())(Reader.string("({e:Type} -> e) -> v")).get()?.pretty() shouldBe "({e:*} -> e) -> v"
    }

    "[parse] e e" {
        (Type() thenLeft eos())(Reader.string("e e")).get()?.pretty() shouldBe "e (e)"
    }

    "[parse] e {e}" {
        (Type() thenLeft eos())(Reader.string("e {e}")).get()?.pretty() shouldBe "e {e}"
    }
})

