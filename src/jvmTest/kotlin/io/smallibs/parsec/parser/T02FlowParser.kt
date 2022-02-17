package lambdada.parsec.parser

import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe
import io.smallibs.parsec.io.Reader
import io.smallibs.parsec.parser.Common.get
import io.smallibs.parsec.parser.Core.any
import io.smallibs.parsec.parser.Core.fails
import io.smallibs.parsec.parser.Core.returns
import io.smallibs.parsec.parser.Flow.or
import io.smallibs.parsec.parser.Flow.then
import io.smallibs.parsec.parser.Flow.thenLeft
import io.smallibs.parsec.parser.Flow.thenRight

class T02FlowParser : StringSpec({

    "shouldSequenceParserReturnsAccept" {
        val parser = returns<Char, Char>('a') then returns(1)

        parser(givenAReader()).fold({ it.value == 'a' to 1 }, { false }) shouldBe true
    }

    "shouldSequenceParserReturnsReject" {
        val parser = any<Char>() then fails<Char, Unit>()

        parser(givenAReader()).fold({ false }, { true }) shouldBe true
    }

    "shouldSequenceParserReturnsAcceptWithLeftValue" {
        val parser = returns<Char, Char>('a') thenLeft returns(1)

        parser(givenAReader()).get() shouldBe 'a'
    }

    "shouldSequenceParserReturnsAcceptWithRightValue" {
        val parser = returns<Char, Char>('a') thenRight returns(1)

        parser(givenAReader()).get() shouldBe 1
    }

    "shouldChoiceParserReturnsAccept" {
        val parser = returns<Char, Char>('a') or returns('b')

        parser(givenAReader()).fold({ it.value == 'a' }, { false }) shouldBe true
    }

    "shouldChoiceWithFailsParserReturnsAccept" {
        val parser = fails<Char, Char>() or returns('b')

        parser(givenAReader()).fold({ it.value == 'b' }, { false }) shouldBe true
    }

}) {
    companion object {
        private fun givenAReader() = Reader.string("an example")
    }
}