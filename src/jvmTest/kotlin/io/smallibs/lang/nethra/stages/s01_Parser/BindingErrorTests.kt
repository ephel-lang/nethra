package io.smallibs.lang.nethra.stages.s01_Parser

import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe
import io.smallibs.parsec.io.Reader
import io.smallibs.parsec.parser.Common.error
import io.smallibs.parsec.parser.Flow.eos
import io.smallibs.parsec.parser.Flow.thenLeft
import io.smallibs.parsec.utils.Location

class BindingErrorSpec : StringSpec({

    "[parser] sig t : { e \\n : Type } -> " {
        (Binding() thenLeft eos())(Reader.string("sig t : { e \n : Type } -> ")).error()?.location shouldBe Location(26, 2, 13)
    }

    "[parser] def t = \\n { e : Type } \\n -> * " {
        (Binding() thenLeft eos())(Reader.string("def t = \n { e : Type } \n -> *")).error()?.location shouldBe Location(29, 3, 5)
    }

})

