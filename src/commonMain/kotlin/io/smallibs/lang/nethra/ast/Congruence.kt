package io.smallibs.lang.nethra.ast

import io.smallibs.lang.nethra.ast.impl.CongruenceImpl

interface Congruence<C> {
    infix fun Term<C>.compatibleWith(i: Term<C>): Boolean

    companion object {
        operator fun <C> invoke(): Congruence<C> = CongruenceImpl()
    }
}