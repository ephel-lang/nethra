package io.smallibs.parsec.parser

import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe
import io.smallibs.parsec.io.Reader
import io.smallibs.parsec.io.tokenize
import io.smallibs.parsec.parser.Common.get
import io.smallibs.parsec.parser.Core.any
import io.smallibs.parsec.parser.Flow.eos
import io.smallibs.parsec.parser.Flow.opt
import io.smallibs.parsec.parser.Flow.rep
import io.smallibs.parsec.parser.Flow.thenLeft
import io.smallibs.parsec.parser.Literal.char
import io.smallibs.parsec.parser.Literal.integer

class T08TokenizerParser : StringSpec({

    "shouldCountNumber" {
        val tokenizer = integer thenLeft char(',').opt
        val reader = Reader.string("42,43").tokenize(tokenizer)
        val parser = any<Int>().rep thenLeft eos()

        (parser(reader).get()?.size ?: -1) shouldBe 2
    }

})