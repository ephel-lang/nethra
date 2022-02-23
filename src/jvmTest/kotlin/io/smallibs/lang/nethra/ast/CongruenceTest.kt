package io.smallibs.lang.nethra.ast

import io.kotest.core.spec.style.StringSpec
import io.smallibs.lang.nethra.extension.MatcherForTerm.shouldBe
import io.smallibs.lang.nethra.extension.MatcherForTerm.shouldNotBe

class CongruenceTest : StringSpec({
    with(Builder<Nothing>()) {
        "should compare same type" {
            type(2) shouldBe type(2)
        }

        "should not compare same type in different levels" {
            type(2) shouldNotBe type(3)
        }

        "should compare same identifier" {
            id("x") shouldBe id("x")
        }

        "should compare different identifiers" {
            id("x") shouldNotBe id("y")
        }

        "should compare Pi based on alpha conversion" {
            pi("x", type(), id("x")) shouldBe pi("y", type(), id("y"))
        }
    }
})
