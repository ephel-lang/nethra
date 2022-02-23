package io.smallibs.lang.nethra.stages.s01_Parser

import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe

class ParserStageTest : StringSpec({

    "[compile/decompile] sig t : Int def t = 1" {
        with(ParserStage()) {
            decompile(compile("sig t : Int def t = 1")) shouldBe "sig t : Int\ndef t = 1"
        }
    }

})