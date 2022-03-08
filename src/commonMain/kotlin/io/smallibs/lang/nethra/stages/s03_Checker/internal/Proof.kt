package io.smallibs.lang.nethra.stages.s03_Checker.internal

import io.smallibs.lang.nethra.ast.Ast.Term

sealed interface Proof<C> {

    data class Context<C>(
        val gamma: Bindings<C>,
        val type: Term<C>,
    )

    sealed interface Step<C>

    data class Check<C>(val context: Context<C>, val term: Term<C>, val type: Term<C>): Step<C>
    data class Checked<C>(val context: Context<C>, val term: Term<C>, val type: Term<C>): Step<C>
    data class NotChecked<C>(val context: Context<C>, val term: Term<C>, val type: Term<C>): Step<C>

    data class Infer<C>(val context: Context<C>, val term: Term<C>): Step<C>
    data class Infered<C>(val context: Context<C>, val term: Term<C>, val type: Term<C>): Step<C>
    data class NotInfered<C>(val context: Context<C>, val term: Term<C>): Step<C>


}