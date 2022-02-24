package io.smallibs.lang.nethra.ast

import io.smallibs.lang.nethra.ast.impl.CongruenceImpl
import io.smallibs.lang.nethra.stages.s03_Checker.internal.Bindings

interface Congruence<C> {
    fun Bindings<C>.congruent(lhd: Ast.Term<C>, rhd: Ast.Term<C>): Boolean

    companion object {
        operator fun <C> invoke(): Congruence<C> = CongruenceImpl()
    }
}