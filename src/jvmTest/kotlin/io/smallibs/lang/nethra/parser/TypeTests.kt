package io.smallibs.lang.nethra.parser

import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe
import io.smallibs.parsec.io.Reader
import io.smallibs.parsec.parser.Common.isSuccess
import io.smallibs.parsec.parser.Flow.eos
import io.smallibs.parsec.parser.Flow.thenLeft

class TypeSpec : StringSpec({

    "should parse an int" {
        (Type() thenLeft eos())(Reader.string("Int")).isSuccess() shouldBe true
    }

    "should parse a char" {
        (Type() thenLeft eos())(Reader.string("Char")).isSuccess() shouldBe true
    }

    "should parse an block" {
        (Type() thenLeft eos())(Reader.string("(Char)")).isSuccess() shouldBe true
    }

    "should parse an function type" {
        (Type() thenLeft eos())(Reader.string("Char -> Int")).isSuccess() shouldBe true
    }

    "should parse a dependant function type" {
        (Type() thenLeft eos())(Reader.string("(e:Char) -> e")).isSuccess() shouldBe true
    }

    "should parse a implicit dependant function type" {
        (Type() thenLeft eos())(Reader.string("{e:Char} -> e")).isSuccess() shouldBe true
    }

    "should parse a complex implicit dependant function type and more" {
        (Type() thenLeft eos())(Reader.string("{e:Type} -> ({v:e} -> v)")).isSuccess() shouldBe true
    }
})

