package io.smallibs.parsec.parser

import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe
import io.smallibs.parsec.io.Reader
import io.smallibs.parsec.parser.Core.`try`
import io.smallibs.parsec.parser.Core.any
import io.smallibs.parsec.parser.Core.not
import io.smallibs.parsec.parser.Flow.eos
import io.smallibs.parsec.parser.Flow.or
import io.smallibs.parsec.parser.Flow.then
import io.smallibs.parsec.parser.Flow.thenLeft
import io.smallibs.parsec.parser.Literal.char
import io.smallibs.parsec.parser.Monad.map
import io.smallibs.parsec.parser.Monad.satisfy

class T04ElementParser : StringSpec({

    "shouldAnyParserReturnsAccept" {
        val parser = any<Char>()

        parser(Reader.string("a")).fold({ it.value == 'a' && it.consumed }, { false }) shouldBe true
    }

    "shouldAnyParserReturnsReject" {
        val parser = any<Char>()

        parser(Reader.string("")).fold({ false }, { true }) shouldBe true
    }

    "shouldEOSParserReturnsAccept" {
        val parser = eos<Char>()

        parser(Reader.string("")).fold({ true }, { false }) shouldBe true
    }

    "shouldEOSParserReturnsReject" {
        val parser = eos<Char>()

        parser(Reader.string("a")).fold({ false }, { true }) shouldBe true
    }

    "shouldChoiceParserReturnsReject" {
        val parser = ((any<Char>() thenLeft any()) or any()) then eos()

        parser(Reader.string("a")).fold({ false }, { true }) shouldBe true
    }

    "shouldChoiceWithBacktrackParserReturnsAccept" {
        val parser = (`try`((any<Char>() then any()).map { it.first }) or any()) then eos()

        parser(Reader.string("a")).fold({ true }, { false }) shouldBe true
    }

    "shouldSatisfyParserReturnsAccept" {
        val parser = any<Char>()

        parser(Reader.string("a")).fold({ it.value == 'a' && it.consumed }, { false }) shouldBe true
    }

    "shouldNotSatisfyOrAnyParserReturnsAccept" {
        val parser = `try`(any<Char>().satisfy { it == 'a' }) or any()

        parser(Reader.string("b")).fold({ it.value == 'b' && it.consumed }, { false }) shouldBe true
    }

    "shouldNotCharParserReturnsAccept" {
        val parser = not(char('a'))

        parser(Reader.string("b")).fold({ it.value == 'b' && it.consumed }, { false }) shouldBe true
    }

    "shouldNotCharParserReturnsReject" {
        val parser = not(char('a'))

        parser(Reader.string("a")).fold({ false }, { true }) shouldBe true
    }
})