package io.smallibs.parsec.io

import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe

class T00CharReader : StringSpec({

    "should return a character" {
        Reader.string("a").read()?.first shouldBe 'a'
    }

    "should return nothing" {
        Reader.string("").read() shouldBe null
    }

    "should return another character" {
        Reader.string("ab").read()?.second?.read()?.first shouldBe 'b'
    }

})