package io.smallibs.parsec.parser

import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe
import io.smallibs.parsec.io.Reader
import io.smallibs.parsec.parser.Flow.eos
import io.smallibs.parsec.parser.Flow.thenLeft
import io.smallibs.parsec.parser.Literal.delimitedChar
import io.smallibs.parsec.parser.Literal.delimitedString
import io.smallibs.parsec.parser.Literal.string

class T07CharParser : StringSpec({

    "shouldDelimitedCharParserReturnChar" {
        val parser = delimitedChar thenLeft eos()

        parser(Reader.string("'a'")).fold({ it.value }, { null }) shouldBe 'a'
    }

    "shouldDelimitedDelimiterCharParserReturnChar" {
        val parser = delimitedChar thenLeft eos()

        parser(Reader.string("'\\\''")).fold({ it.value }, { null }) shouldBe '\''
    }
})