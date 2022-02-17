package io.smallibs.lang.nethra.checker

import io.smallibs.lang.nethra.ast.Term
import io.smallibs.lang.nethra.checker.impl.InferenceImpl

interface Inference<C> {
    fun Gamma<C>.infer(term: Term<C>): Term<C>

    companion object {
        operator fun <C> invoke(checker: Checker<C>): Inference<C> = InferenceImpl(checker)
    }
}