package io.smallibs.parsec.parser

import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe
import io.smallibs.parsec.io.Reader
import io.smallibs.parsec.parser.Flow.eos
import io.smallibs.parsec.parser.Flow.thenLeft
import io.smallibs.parsec.parser.Literal.delimitedString
import io.smallibs.parsec.parser.Literal.string

class T07StringParser : StringSpec({

    "shouldStringParserReturnAccept" {
        val parser = string("hello") thenLeft eos()

        parser(Reader.string("hello")).fold({ it.value == "hello" }, { false }) shouldBe true
    }


    "shouldDelimitedStringParserReturnEmptyString" {
        val parser = delimitedString() thenLeft eos()

        parser(Reader.string(""""hel\"lo"""")).fold({ it.value }, { null }) shouldBe """hel\"lo"""
    }


    "shouldStringWithMetaCharacterParserReturnAccept" {
        val parser = string("hel\\nlo") thenLeft eos()

        parser(Reader.string("hel\\nlo")).fold({ it.value == "hel\\nlo" }, { false }) shouldBe true
    }
})