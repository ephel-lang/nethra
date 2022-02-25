package io.smallibs.lang.nethra.stages.s01_Parser

import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe

class ParserStageTest : StringSpec({

    "[compile/decompile] sig t : int def t = 1" {
        with(ParserStage()) {
            decompile(compile("sig t : int def t = 1")) shouldBe "sig t : int\ndef t = 1"
        }
    }

})