package io.smallibs.lang.nethra.stages.s01_Parser

import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe
import io.smallibs.lang.nethra.cst.Cst.prettyBinding
import io.smallibs.lang.nethra.stages.s01_Parser.internal.BindingParser
import io.smallibs.parsec.io.Reader.Companion.string
import io.smallibs.parsec.parser.Common.get
import io.smallibs.parsec.parser.Flow.eos
import io.smallibs.parsec.parser.Flow.thenLeft

class BindingNominalSpec : StringSpec({

    "[parser] sig test : Int -> Int " {
        (BindingParser() thenLeft eos())(string("sig test : Int -> Int")).get()?.prettyBinding() shouldBe "sig test : Int -> Int"
    }

    "[parser] def test = Int -> Int " {
        (BindingParser() thenLeft eos())(string("def test = Int -> Int")).get()?.prettyBinding() shouldBe "def test = Int -> Int"
    }

})

