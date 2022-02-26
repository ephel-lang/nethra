package io.smallibs.lang.nethra.stages.s03_Checker.internal

import io.smallibs.lang.nethra.ast.Ast
import io.smallibs.lang.nethra.ast.Printer

class Bindings<C>(
    val gamma: Map<String, Ast.Term<C>>,
    val delta: Map<String, Ast.Term<C>>,
    private val printer: Printer<C>,
) {
    fun getDefinition(id: String): Ast.Term<C>? = delta[id]

    fun getSignature(id: String): Ast.Term<C>? = gamma[id]

    fun setSignature(id: String, term: Ast.Term<C>): Bindings<C> = Bindings(mapOf(id to term) + gamma, delta)

    fun prettyPrint(): String = with(printer) {
        // " Γ"
        gamma.entries.joinToString(",", transform = { e -> " ${e.key} : ${e.value.prettyPrint()}" })
        // delta.entries.joinToString(",", transform = { e -> " ${e.key} = ${e.value.prettyPrint()}" })
    }

    companion object {
        operator fun <C> invoke(
            gamma: Map<String, Ast.Term<C>> = emptyMap(),
            delta: Map<String, Ast.Term<C>> = emptyMap(),
        ): Bindings<C> =
            mapOf<String, Ast.Term<C>>(
                "int" to Ast.Term.Type(),
                "string" to Ast.Term.Type(),
                "char" to Ast.Term.Type()
            ).let { basicGamma ->
                Bindings(basicGamma + gamma, delta, Printer())
            }
    }

}
