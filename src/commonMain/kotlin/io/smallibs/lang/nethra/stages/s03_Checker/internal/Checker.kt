package io.smallibs.lang.nethra.stages.s03_Checker.internal

import io.smallibs.lang.nethra.ast.Ast
import io.smallibs.lang.nethra.stages.s03_Checker.internal.impl.CheckerImpl

interface Checker<C> {
    fun Bindings<C>.check(term: Ast.Term<C>, type: Ast.Term<C>): Proof<C>

    companion object {
        operator fun <C> invoke(): Checker<C> = CheckerImpl()
    }
}