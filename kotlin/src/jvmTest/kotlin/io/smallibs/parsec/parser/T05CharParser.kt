package io.smallibs.parsec.parser

import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe
import io.smallibs.parsec.io.Reader
import io.smallibs.parsec.parser.Flow.or
import io.smallibs.parsec.parser.Literal.char
import io.smallibs.parsec.parser.Literal.charIn

class T05CharParser : StringSpec({

    "shouldCharParserReturnsAccept" {
        val parser = char('a')

        parser(Reader.string("a")).fold({ it.value == 'a' }, { false }) shouldBe true
    }

    "shouldCharParserReturnsFails" {
        val parser = char('a')

        parser(Reader.string("b")).fold({ false }, { true }) shouldBe true
    }

    "shouldCharInRangeParserReturnsAccept" {
        val parser = charIn('a'..'z')

        parser(Reader.string("a")).fold({ it.value == 'a' }, { false }) shouldBe true
    }

    "shouldCharInRangeParserReturnsFails" {
        val parser = charIn('b'..'z')

        parser(Reader.string("a")).fold({ false }, { true }) shouldBe true
    }

    "shouldCharInStringParserReturnsAccept" {
        val parser = charIn("-+")

        parser(Reader.string("-")).fold({ it.value == '-' }, { false }) shouldBe true
    }

    "shouldCharInStringParserReturnsFails" {
        val parser = charIn("-+")

        parser(Reader.string("/")).fold({ false }, { true }) shouldBe true
    }

    "shouldCharParserOrParserReturnsAccept" {
        val parser = char('a') or char('b')

        parser(Reader.string("b")).fold({ it.value == 'b' }, { false }) shouldBe true
    }
})