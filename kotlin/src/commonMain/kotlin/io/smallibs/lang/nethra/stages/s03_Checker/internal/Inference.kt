package io.smallibs.lang.nethra.stages.s03_Checker.internal

import io.smallibs.lang.nethra.ast.Ast
import io.smallibs.lang.nethra.stages.s03_Checker.internal.impl.InferenceImpl

interface Inference<C> {
    fun Bindings<C>.infer(term: Ast.Term<C>): Pair<Ast.Term<C>?, Proof<C>>

    companion object {
        operator fun <C> invoke(checker: Checker<C>): Inference<C> = InferenceImpl(checker)
    }
}