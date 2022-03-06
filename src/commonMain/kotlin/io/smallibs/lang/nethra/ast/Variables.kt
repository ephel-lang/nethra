package io.smallibs.lang.nethra.ast

import io.smallibs.lang.nethra.ast.impl.FreeVariableImpl

interface Variables<C> {

    fun free(t: Ast.Term<C>): Set<String>

    companion object {
        operator fun <C> invoke(): Variables<C> = FreeVariableImpl()
    }

}