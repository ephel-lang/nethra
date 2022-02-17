package io.smallibs.lang.nethra.ast

import io.smallibs.lang.nethra.ast.impl.SubstitutionImpl

interface Substitution<C> {
    fun newVariable(): String
    fun Term<C>.substitute(name: String, term: Term<C>): Term<C>

    companion object {
        operator fun <C> invoke(): Substitution<C> = SubstitutionImpl()
    }
}