package io.smallibs.lang.nethra.parser

import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe
import io.smallibs.parsec.io.Reader
import io.smallibs.parsec.parser.Common.error
import io.smallibs.parsec.parser.Flow.eos
import io.smallibs.parsec.parser.Flow.thenLeft
import io.smallibs.parsec.utils.Location

class ErrorTypeSpec : StringSpec({

    "[parser] Int -> " {
        (Term() thenLeft eos())(Reader.string("Int -> ")).error()?.location shouldBe Location(7, 0, 7)
    }

    "[parser] Int -> * " {
        (Term() thenLeft eos())(Reader.string("Int -> * ")).error()?.location shouldBe Location(8, 0, 8)
    }

    "[parser] { e }" {
        (Term() thenLeft eos())(Reader.string("{ e }")).error()?.location shouldBe Location(5, 0, 5)
    }

    "[parser] e { v : Type } -> v" {
        (Term() thenLeft eos())(Reader.string("e { v : Type } -> v")).error()?.location shouldBe Location(2, 0, 2)
    }
})

