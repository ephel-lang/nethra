package io.smallibs.lang.nethra.stages.s03_Checker.internal

import io.smallibs.lang.nethra.ast.Ast
import io.smallibs.lang.nethra.stages.s03_Checker.internal.impl.InferenceImpl

interface Inference<C> {
    fun Context<C>.infer(term: Ast.Term<C>): Ast.Term<C>

    companion object {
        operator fun <C> invoke(checker: Checker<C>): Inference<C> = InferenceImpl(checker)
    }
}