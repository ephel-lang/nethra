package io.smallibs.lang.nethra.ast.impl

import io.smallibs.lang.nethra.ast.Ast
import io.smallibs.lang.nethra.ast.Ast.Term.Apply
import io.smallibs.lang.nethra.ast.Ast.Term.Hole
import io.smallibs.lang.nethra.ast.Ast.Term.Id
import io.smallibs.lang.nethra.ast.Ast.Term.Inl
import io.smallibs.lang.nethra.ast.Ast.Term.Inr
import io.smallibs.lang.nethra.ast.Builder
import io.smallibs.lang.nethra.ast.Printer
import io.smallibs.lang.nethra.ast.Reducer
import io.smallibs.lang.nethra.ast.Substitution
import io.smallibs.lang.nethra.stages.s03_Checker.internal.Bindings

class ReducerImpl<C>(
    private val substitution: Substitution<C> = Substitution(),
    private val builder: Builder<C> = Builder(),
    private val printer: Printer<C> = Printer(),
) : Reducer<C>, Printer<C> by printer, Substitution<C> by substitution {

    override fun Bindings<C>.reduce(t: Ast.Term<C>): Ast.Term<C> = //println("[?] ${t.prettyPrint()} *→ ?").let {
        Unit.let {
            when (t) {
                is Hole -> t.term?.let { reduce(it) } ?: t
                is Id -> this.getDefinition(t.value)?.let { reduce(it) } ?: t
                is Apply -> when (val abstraction = reduce(t.abstraction)) {
                    is Ast.Term.Lambda -> reduce(abstraction.body.substitute(abstraction.n, t.argument))
                    else -> t
                }
                is Ast.Term.Case -> when (val proj = reduce(t.term)) {
                    is Inl -> reduce(builder.apply(t.left, proj.term))
                    is Inr -> reduce(builder.apply(t.right, proj.term))
                    else -> t
                }
                is Ast.Term.Fst -> when (val t = reduce(t.term)) {
                    is Ast.Term.Sigma -> reduce(t.bound)
                    else -> t
                }
                is Ast.Term.Snd -> when (val t = reduce(t.term)) {
                    is Ast.Term.Sigma -> reduce(t.body.substitute(t.n, t.bound))
                    else -> t
                }
                else -> t
            }
        }.let {
            // println("[?] ${t.prettyPrint()} *→ ${it.prettyPrint()}")
            it
        }

}