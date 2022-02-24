package io.smallibs.lang.nethra.extension

import io.kotest.matchers.shouldBe
import io.smallibs.lang.nethra.ast.Ast
import io.smallibs.lang.nethra.ast.Congruence
import io.smallibs.lang.nethra.stages.s03_Checker.internal.Bindings

object MatcherForTerm {
    infix fun <C> Ast.Term<C>.shouldBe(term: Ast.Term<C>): Unit = with(Congruence<C>()) {
        Bindings<C>().congruent(this@shouldBe, term) shouldBe true
    }

    infix fun <C> Ast.Term<C>.shouldNotBe(term: Ast.Term<C>): Unit = with(Congruence<C>()) {
        Bindings<C>().congruent(this@shouldNotBe, term) shouldBe false
    }
}