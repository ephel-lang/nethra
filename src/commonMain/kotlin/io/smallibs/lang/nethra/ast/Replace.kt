package io.smallibs.lang.nethra.ast

import io.smallibs.lang.nethra.ast.impl.ReplaceImpl

interface Replace<C> {
    fun Ast.Term<C>.replace(param: Pair<Ast.Term<C>, Ast.Term<C>>): Ast.Term<C>

    companion object {
        operator fun <C> invoke(): Replace<C> = ReplaceImpl()
    }
}