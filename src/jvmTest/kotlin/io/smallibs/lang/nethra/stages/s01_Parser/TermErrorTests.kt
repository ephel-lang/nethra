package io.smallibs.lang.nethra.stages.s01_Parser

import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe
import io.smallibs.parsec.io.Reader
import io.smallibs.parsec.parser.Common.error
import io.smallibs.parsec.parser.Flow.eos
import io.smallibs.parsec.parser.Flow.thenLeft
import io.smallibs.parsec.utils.Location

class TermErrorSpec : StringSpec({

    "[parser] Int -> " {
        (Term() thenLeft eos())(Reader.string("Int -> ")).error()?.location shouldBe Location(7, 1, 7)
    }

    "[parser] Int -> * " {
        (Term() thenLeft eos())(Reader.string("Int -> * ")).error()?.location shouldBe Location(8, 1, 8)
    }

    "[parser] { e }" {
        (Term() thenLeft eos())(Reader.string("{ e }")).error()?.location shouldBe Location(5, 1, 5)
    }

    "[parser] { e \\n }" {
        (Term() thenLeft eos())(Reader.string("{ e \n }")).error()?.location shouldBe Location(7, 2, 2)
    }

    "[parser] e { v : Type } -> v" {
        (Term() thenLeft eos())(Reader.string("e { v : Type } -> v")).error()?.location shouldBe Location(2, 1, 2)
    }

})

