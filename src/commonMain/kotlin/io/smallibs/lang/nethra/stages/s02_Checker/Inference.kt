package io.smallibs.lang.nethra.stages.s02_Checker

import io.smallibs.lang.nethra.ast.Term
import io.smallibs.lang.nethra.stages.s02_Checker.impl.InferenceImpl

interface Inference<C> {
    fun Gamma<C>.infer(term: Term<C>): Term<C>

    companion object {
        operator fun <C> invoke(checker: Checker<C>): Inference<C> = InferenceImpl(checker)
    }
}