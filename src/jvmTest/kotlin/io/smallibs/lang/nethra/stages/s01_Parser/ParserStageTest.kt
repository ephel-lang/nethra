package io.smallibs.lang.nethra.stages.s01_Parser

import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe
import io.smallibs.lang.nethra.stages.report.ErrorReporter

class ParserStageTest : StringSpec({

    "[compile/decompile] sig t : int def t = 1" {
        "sig t : int def t = 1".let { source ->
            with(ParserStage(ErrorReporter(source))) {
                decompile(act(source)) shouldBe "sig t : int\ndef t = 1"
            }
        }
    }

})