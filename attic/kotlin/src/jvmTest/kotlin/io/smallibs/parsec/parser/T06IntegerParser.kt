package io.smallibs.parsec.parser

import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe
import io.smallibs.parsec.io.Reader
import io.smallibs.parsec.parser.Flow.eos
import io.smallibs.parsec.parser.Flow.thenLeft
import io.smallibs.parsec.parser.Literal.integer

class T06IntegerParser : StringSpec({

    "shouldPositiveIntegerParserReturnAccept" {
        val parser = integer thenLeft eos()

        parser(Reader.string("+42")).fold({ it.value == 42 }, { false }) shouldBe true
    }

    "shouldNegativeIntegerParserReturnAccept" {
        val parser = integer thenLeft eos()

        parser(Reader.string("-42")).fold({ it.value == -42 }, { false }) shouldBe true
    }

})