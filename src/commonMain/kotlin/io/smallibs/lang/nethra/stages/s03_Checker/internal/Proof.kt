package io.smallibs.lang.nethra.stages.s03_Checker.internal

import io.smallibs.lang.nethra.ast.Ast.Term
import io.smallibs.lang.nethra.stages.report.CompilationException

sealed interface Proof<C> {

    sealed interface Goal<C>

    data class Check<C>(
        val bindings: Bindings<C>,
        val term: Term<C>,
        val type: Term<C>,
    ) : Goal<C>

    data class Infer<C>(
        val bindings: Bindings<C>,
        val term: Term<C>,
        val type: Term<C>?,
    ) : Goal<C>

    data class Congruent<C>(
        val bindings: Bindings<C>,
        val term: Term<C>,
        val expected: Term<C>,
        val computed: Term<C>,
    ) : Goal<C>

    fun success(): Boolean

    fun error()

    data class Step<C>(val conclusion: Goal<C>, val premisses: List<Proof<C>> = listOf()) : Proof<C> {
        override fun success(): Boolean = premisses.all { it.success() }
        override fun error() {
            if (premisses.filter { it is Failure<C> }.isNotEmpty()) {
                when (conclusion) {
                    is Check -> throw CompilationException.CannotCheck(conclusion.term, conclusion.type)
                    is Congruent -> throw CompilationException.CannotCompare(conclusion.term,
                        conclusion.expected,
                        conclusion.computed)
                    is Infer -> throw CompilationException.CannotInfer(conclusion.term)
                }
            } else {
                premisses.forEach { it.error() }
            }
        }
    }

    data class Failure<C>(val reason: CompilationException? = null) : Proof<C> {
        override fun success(): Boolean = false
        override fun error() {
            reason?.let { throw it }
        }
    }
}