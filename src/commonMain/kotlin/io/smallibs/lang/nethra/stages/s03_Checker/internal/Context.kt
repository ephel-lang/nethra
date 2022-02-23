package io.smallibs.lang.nethra.stages.s03_Checker.internal

import io.smallibs.lang.nethra.ast.Ast
import io.smallibs.lang.nethra.ast.Printer

class Context<C>(
    private val gamma: Map<String, Ast.Term<C>> = emptyMap(),
    private val delta: Map<String, Ast.Term<C>> = emptyMap(),
    private val printer: Printer<C> = Printer(),
) {
    fun getDefinition(id: String): Ast.Term<C>? =
        gamma[id]

    fun getSignature(id: String): Ast.Term<C>? =
        gamma[id]

    fun setSignature(id: String, term: Ast.Term<C>): Context<C> =
        Context(mapOf(id to term) + gamma)

    fun prettyPrint(): String = with(printer) {
        " Γ"
        //gamma.entries.joinToString(",", transform = { e -> " ${e.key} : ${e.value.prettyPrint()}" })
    }
}
