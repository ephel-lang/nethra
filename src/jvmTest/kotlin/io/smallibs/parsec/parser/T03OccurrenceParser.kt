package lambdada.parsec.parser

import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe
import io.smallibs.parsec.io.Reader
import io.smallibs.parsec.parser.Core.any
import io.smallibs.parsec.parser.Core.not
import io.smallibs.parsec.parser.Flow.eos
import io.smallibs.parsec.parser.Flow.opt
import io.smallibs.parsec.parser.Flow.optrep
import io.smallibs.parsec.parser.Flow.rep
import io.smallibs.parsec.parser.Flow.then
import io.smallibs.parsec.parser.Flow.thenLeft
import io.smallibs.parsec.parser.Literal.char

class T03OccurrenceParser : StringSpec({

    "shouldOptionalParserWithEmptyStringReturnsAccept" {
        val parser = any<Char>().opt then eos()

        parser(Reader.string("")).fold({ it.value.first == null }, { false }) shouldBe true
    }

    "shouldOptionalParserWithNonEmptyStringReturnsAccept" {
        val parser = any<Char>().opt then eos()

        parser(Reader.string("a")).fold({ it.value.first == 'a' }, { false }) shouldBe true
    }

    "shouldOptionalRepeatableParserWithEmptyStringReturnsAccept" {
        val parser = any<Char>().optrep then eos()

        parser(Reader.string("")).fold({ true }, { false }) shouldBe true
    }

    "shouldOptionalRepeatableParserWithNonEmptyStringReturnsAccept" {
        val parser = any<Char>().optrep then eos()

        parser(Reader.string("ab")).fold({ it.value.first == listOf('a', 'b') }, { false }) shouldBe true
    }

    "shouldRepeatableParserWithEmptyStringReturnsReject" {
        val parser = any<Char>().rep then eos()

        parser(Reader.string("")).fold({ false }, { true }) shouldBe true
    }

    "shouldRepeatableParserWithNonEmptyStringReturnsAccept" {
        val parser = any<Char>().rep then eos()

        parser(Reader.string("ab")).fold({ it.value.first == listOf('a', 'b') }, { false }) shouldBe true
    }

    "shouldRepeatableNotParserWithNonEmptyStringReturnsAccept" {
        val parser = not(char('a')).rep then eos()

        parser(Reader.string("bc")).fold({ it.value.first == listOf('b', 'c') }, { false }) shouldBe true
    }

    "shouldRepeatableNotThenCharParserWithNonEmptyStringReturnsAccept" {
        val parser = not(char('a')).optrep

        parser(Reader.string("bca")).fold({ it.value == listOf('b', 'c') }, { false }) shouldBe true
    }

    "shouldBeAbleToParseLargeInput" {
        val parser = any<Char>().optrep thenLeft eos()

        val size = 16 * 1024
        val content = "a".repeat(size)

        parser(Reader.string(content)).fold({ it.value.size }, { 0 }) shouldBe size
    }

})