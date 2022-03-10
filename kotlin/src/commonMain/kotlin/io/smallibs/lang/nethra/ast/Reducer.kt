package io.smallibs.lang.nethra.ast

import io.smallibs.lang.nethra.ast.impl.ReducerImpl
import io.smallibs.lang.nethra.stages.s03_Checker.internal.Bindings

interface Reducer<C> {
    fun Bindings<C>.reduce(t: Ast.Term<C>): Ast.Term<C>

    companion object {
        operator fun <C> invoke(): Reducer<C> = ReducerImpl()
    }
}