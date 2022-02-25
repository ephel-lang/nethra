package io.smallibs.lang.nethra.stages.s03_Checker.internal

import io.smallibs.lang.nethra.ast.Ast
import io.smallibs.lang.nethra.ast.Printer

class Bindings<C>(
    val gamma: Map<String, Ast.Term<C>> = emptyMap(),
    val delta: Map<String, Ast.Term<C>> = emptyMap(),
    private val printer: Printer<C> = Printer(),
) {
    fun getDefinition(id: String): Ast.Term<C>? = delta[id]

    fun getSignature(id: String): Ast.Term<C>? = gamma[id]

    fun setSignature(id: String, term: Ast.Term<C>): Bindings<C> = Bindings(mapOf(id to term) + gamma, delta)

    fun prettyPrint(): String = with(printer) {
        " Γ"
        // gamma.entries.joinToString(",", transform = { e -> " ${e.key} : ${e.value.prettyPrint()}" }) +
        // delta.entries.joinToString(",", transform = { e -> " ${e.key} = ${e.value.prettyPrint()}" })
    }
}
