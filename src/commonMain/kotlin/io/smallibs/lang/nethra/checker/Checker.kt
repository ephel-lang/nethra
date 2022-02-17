package io.smallibs.lang.nethra.checker

import io.smallibs.lang.nethra.ast.Term
import io.smallibs.lang.nethra.checker.impl.CheckerImpl

interface Checker<C> {
    fun Gamma<C>.check(term: Term<C>, type: Term<C>): Boolean

    companion object {
        operator fun <C> invoke(): Checker<C> = CheckerImpl()
    }
}