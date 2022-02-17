package io.smallibs.parsec.parser

import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe
import io.smallibs.parsec.io.Reader
import io.smallibs.parsec.parser.Common.get
import io.smallibs.parsec.parser.Common.isSuccess
import io.smallibs.parsec.parser.Core.any
import io.smallibs.parsec.parser.Core.fails
import io.smallibs.parsec.parser.Core.lazy
import io.smallibs.parsec.parser.Core.not
import io.smallibs.parsec.parser.Core.returns

class T00CoreParser : StringSpec({
    "shouldReturnsParserReturnsAccept" {
        val parser: Parser<Char, Boolean> = returns(true)

        parser.invoke(givenAReader()).isSuccess() shouldBe true
    }

    "shouldFailsParserReturnsError" {
        val parser = fails<Char, Char>()

        parser.invoke(givenAReader()).isSuccess() shouldBe false
    }

    "shouldLazyReturnsParserReturnsAccept" {
        val parser = lazy { returns<Char, Char>('a') }

        parser(givenAReader()).get() shouldBe 'a'
    }

    "shouldLazyFailsParserReturnsError" {
        val parser = lazy { fails<Char, Char>() }

        parser(givenAReader()).fold({ false }, { true }) shouldBe true
    }

    "shouldAnyReturnsSuccess" {
        val parser = any<Char>()

        parser(givenAReader("a")).fold({ true }, { false }) shouldBe true
    }

    "shouldAnyReturnsReject" {
        val parser = any<Char>()

        parser(givenAReader()).fold({ false }, { true }) shouldBe true
    }

    "shouldNotReturnsReject" {
        val parser = not(any<Char>())

        parser.invoke(givenAReader("a")).fold({ false }, { true }) shouldBe true
    }
}) {
    companion object {
        private fun givenAReader(s: String = "") = Reader.string(s)
    }
}
