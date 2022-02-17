package io.smallibs.lang.nethra.checker

import io.smallibs.lang.nethra.ast.Printer
import io.smallibs.lang.nethra.ast.Term

class Gamma<C>(private val entries: Map<String, Term<C>> = emptyMap(), private val printer: Printer<C> = Printer()) {
    fun get(id: String): Term<C>? =
        entries[id]

    fun set(id: String, term: Term<C>): Gamma<C> =
        Gamma(mapOf(id to term) + entries)

    fun prettyPrint(): String = with(printer) {
        entries.entries.joinToString(",", transform = { e -> " ${e.key} : ${e.value.prettyPrint()}" })
    }
}
