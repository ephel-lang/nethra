package io.smallibs.lang.nethra.stages.s02_Checker

import io.smallibs.lang.nethra.ast.Term
import io.smallibs.lang.nethra.stages.s02_Checker.impl.CheckerImpl

interface Checker<C> {
    fun Gamma<C>.check(term: Term<C>, type: Term<C>): Boolean

    companion object {
        operator fun <C> invoke(): Checker<C> = CheckerImpl()
    }
}