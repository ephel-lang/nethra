package io.smallibs.parsec.parser

import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe
import io.smallibs.parsec.io.Reader
import io.smallibs.parsec.parser.Common.get
import io.smallibs.parsec.parser.Common.isSuccess
import io.smallibs.parsec.parser.Core.any
import io.smallibs.parsec.parser.Core.fails
import io.smallibs.parsec.parser.Core.returns
import io.smallibs.parsec.parser.Monad.applicative
import io.smallibs.parsec.parser.Monad.bind
import io.smallibs.parsec.parser.Monad.join
import io.smallibs.parsec.parser.Monad.map
import io.smallibs.parsec.parser.Monad.satisfy
import io.smallibs.parsec.parser.Monad.then

class T01MonadParser : StringSpec({
    "shouldMappedReturnsParserReturnsAccept" {
        val parser = returns<Char, Char>('a').map { it -> it == 'a' }

        parser.invoke(givenAReader()).get() shouldBe true
    }

    "shouldJoinReturnedReturns" {
        val parser = join(returns(any<Char>()))

        parser.invoke(givenAReader()).get() shouldBe 'a'
    }

    "shouldJoinReturnedReturnsWithConsumed" {
        val parser = join(returns(any<Char>()))

        parser(givenAReader()).consumed shouldBe true
    }

    "shouldJoinReturnedReturnsWithoutConsumed" {
        val parser = join(returns(returns<Char, Char>('a')))

        parser(givenAReader()).consumed shouldBe false
    }


    "shouldFlapMappedReturnsParserReturnsAccept" {
        val parser = (any<Char>() bind { returns(it + "b") }).map { it == "ab" }

        parser.invoke(givenAReader()).get() shouldBe true
    }

    "shouldFlapMappedReturnsParserReturnsConsumed" {
        val parser = (any<Char>() bind { returns(it + "b") }).map { it == "ab" }

        parser(givenAReader()).consumed shouldBe true
    }

    "shouldFlapMappedReturnsParserReturnsError" {
        val parser = returns<Char, Char>('a').bind { fails<Char, Char>() }

        parser(givenAReader()).isSuccess() shouldBe false
    }

    "shouldApplicativeReturnsAccept" {
        val parser = any<Char>() applicative returns { it: Char -> it == 'a' }

        parser(givenAReader()).isSuccess() shouldBe true
    }

    "shouldApplicativeReturnsAcceptAndConsume" {
        val parser = any<Char>() applicative returns { it: Char -> it == 'a' }

        parser(givenAReader()).consumed shouldBe true
    }

    "shouldApplicativeReturnsReject" {
        val parser = any<Char>() applicative fails<Char, (Char) -> Boolean>()

        parser(givenAReader()).isSuccess() shouldBe false
    }

    "shouldApplicativeReturnsRejectAndConsume" {
        val parser = any<Char>() applicative fails<Char, (Char) -> Boolean>()

        parser(givenAReader()).consumed shouldBe true
    }

    "shouldThenReturnsAccept" {
        val waitFor = { a: Char -> any<Char>() satisfy { it == a } }

        (waitFor then waitFor)('a')(Reader.string("aa")).isSuccess() shouldBe true
    }

    "shouldThenReturnsReject" {
        val waitFor = { a: Char -> any<Char>() satisfy { it == a } }

        (waitFor then waitFor)('a')(Reader.string("ab")).isSuccess() shouldBe false
    }
}) {
    companion object {
        private fun givenAReader() = Reader.string("an example")

    }
}
