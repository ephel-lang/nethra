package io.smallibs.lang.nethra.ast

import io.kotest.core.spec.style.StringSpec
import io.smallibs.lang.extension.Multi
import io.smallibs.lang.nethra.extension.MatcherForTerm.shouldBe

class SubstitutionTest : StringSpec({
    Multi.with(Builder<Nothing>(), Substitution<Nothing>()) {
        {
            "substitute an id" {
                id("x").substitute("x", id("y")) shouldBe id("y")
            }
            "don't substitute an id" {
                id("x").substitute("z", id("y")) shouldBe id("x")
            }
        }
    }
})