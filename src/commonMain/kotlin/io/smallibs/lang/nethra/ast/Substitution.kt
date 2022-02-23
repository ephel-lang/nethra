package io.smallibs.lang.nethra.ast

import io.smallibs.lang.nethra.ast.impl.SubstitutionImpl

interface Substitution<C> {
    fun newVariable(): String
    fun Ast.Term<C>.substitute(name: String, term: Ast.Term<C>): Ast.Term<C>

    companion object {
        operator fun <C> invoke(): Substitution<C> = SubstitutionImpl()
    }
}