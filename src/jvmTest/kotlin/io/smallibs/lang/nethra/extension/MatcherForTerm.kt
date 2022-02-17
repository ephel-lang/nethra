package io.smallibs.lang.nethra.extension

import io.kotest.matchers.shouldBe
import io.smallibs.lang.nethra.ast.Congruence
import io.smallibs.lang.nethra.ast.Term

object MatcherForTerm {
    infix fun <C> Term<C>.shouldBe(term: Term<C>): Unit = with(Congruence<C>()) {
        this@shouldBe compatibleWith term shouldBe true
    }

    infix fun <C> Term<C>.shouldNotBe(term: Term<C>): Unit = with(Congruence<C>()) {
        this@shouldNotBe compatibleWith term shouldBe false
    }
}