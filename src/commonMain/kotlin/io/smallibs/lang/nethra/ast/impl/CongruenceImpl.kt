package io.smallibs.lang.nethra.ast.impl

import io.smallibs.lang.nethra.ast.Ast
import io.smallibs.lang.nethra.ast.Ast.Term.Data
import io.smallibs.lang.nethra.ast.Ast.Term.Id
import io.smallibs.lang.nethra.ast.Ast.Term.Type
import io.smallibs.lang.nethra.ast.Builder
import io.smallibs.lang.nethra.ast.Congruence
import io.smallibs.lang.nethra.ast.Printer
import io.smallibs.lang.nethra.ast.Substitution
import io.smallibs.lang.nethra.ast.Visitor
import io.smallibs.lang.nethra.stages.s03_Checker.internal.Context

//
// Stupid version based on strict equality for the moment
//

class CongruenceImpl<C>(
    private val substitution: Substitution<C> = Substitution(),
    private val builder: Builder<C> = Builder(),
    private val printer: Printer<C> = Printer(),
) : Visitor<C, Pair<Context<C>, Ast.Term<C>>, Boolean>, Congruence<C>, Printer<C> by printer,
    Substitution<C> by substitution {

    override fun Context<C>.congruent(lhd: Ast.Term<C>, rhd: Ast.Term<C>) =
        println("[?] ${lhd.prettyPrint()} ≅ ${rhd.prettyPrint()} / ?").let {
            val r = if (lhd.isHole() || !rhd.isHole()) lhd.reduce().run(this to rhd.reduce())
            else rhd.reduce().run(this to lhd.reduce())
            println("[?] ${lhd.prettyPrint()} ≅ ${rhd.prettyPrint()} / $r")
            r
        }

    private fun Ast.Term<C>.isHole(): Boolean = this is Ast.Term.Hole<C>

    /**
     * Interpret implementation
     */

    override fun Type<C>.run(i: Pair<Context<C>, Ast.Term<C>>) = this == i.second
    override fun Data<C>.run(i: Pair<Context<C>, Ast.Term<C>>) = this == i.second
    override fun Id<C>.run(i: Pair<Context<C>, Ast.Term<C>>) = this == i.second
    override fun Ast.Term.Lit<C>.run(i: Pair<Context<C>, Ast.Term<C>>) = this == i.second

    override fun Ast.Term.Pi<C>.run(i: Pair<Context<C>, Ast.Term<C>>) = when (val t = i.second) {
        is Ast.Term.Pi -> i.first.congruent(this.bound, t.bound) && i.first.withBindingEquals(this.n to this.body,
            t.n to t.body)
        else -> false
    }

    override fun Ast.Term.Lambda<C>.run(i: Pair<Context<C>, Ast.Term<C>>) = when (val t = i.second) {
        is Ast.Term.Lambda -> i.first.withBindingEquals(this.n to this.body, t.n to t.body)
        else -> false
    }

    override fun Ast.Term.Apply<C>.run(i: Pair<Context<C>, Ast.Term<C>>) = when (val t = i.second) {
        is Ast.Term.Apply -> i.first.congruent(this.abstraction, t.abstraction) && i.first.congruent(this.argument,
            t.argument)
        else -> false
    }

    override fun Ast.Term.Sigma<C>.run(i: Pair<Context<C>, Ast.Term<C>>) = when (val t = i.second) {
        is Ast.Term.Sigma -> i.first.withBindingEquals(this.n to this.body, t.n to t.body)
        else -> false
    }

    override fun Ast.Term.Couple<C>.run(i: Pair<Context<C>, Ast.Term<C>>) = when (val t = i.second) {
        is Ast.Term.Couple -> i.first.congruent(this.lhd, t.lhd) && i.first.congruent(this.rhd, t.rhd)
        else -> false
    }

    override fun Ast.Term.Fst<C>.run(i: Pair<Context<C>, Ast.Term<C>>) = this == i.second

    override fun Ast.Term.Snd<C>.run(i: Pair<Context<C>, Ast.Term<C>>) = this == i.second

    override fun Ast.Term.Disjunction<C>.run(i: Pair<Context<C>, Ast.Term<C>>) = when (val t = i.second) {
        is Ast.Term.Disjunction -> i.first.congruent(this.lhd, t.lhd) && i.first.congruent(this.rhd, t.rhd)
        else -> false
    }

    override fun Ast.Term.Inl<C>.run(i: Pair<Context<C>, Ast.Term<C>>) = this == i

    override fun Ast.Term.Inr<C>.run(i: Pair<Context<C>, Ast.Term<C>>) = this == i

    override fun Ast.Term.Case<C>.run(i: Pair<Context<C>, Ast.Term<C>>) = TODO()

    override fun Ast.Term.Rec<C>.run(i: Pair<Context<C>, Ast.Term<C>>) = when (val t = i.second) {
        is Ast.Term.Rec -> i.first.withBindingEquals(this.self to this.body, t.self to t.body)
        else -> false
    }

    override fun Ast.Term.Fold<C>.run(i: Pair<Context<C>, Ast.Term<C>>) = this == i.second

    override fun Ast.Term.Unfold<C>.run(i: Pair<Context<C>, Ast.Term<C>>) = this == i.second

    override fun Ast.Term.Inhabit<C>.run(i: Pair<Context<C>, Ast.Term<C>>) = this == i.second

    override fun Ast.Term.Hole<C>.run(i: Pair<Context<C>, Ast.Term<C>>) = when (term) {
        null -> {
            term = i.second
            true
        }
        else -> term!!.run(i)
    }

    /**
     * Private behaviors
     */

    private fun Context<C>.withBindingEquals(lhd: Pair<String, Ast.Term<C>>, rhd: Pair<String, Ast.Term<C>>): Boolean =
        with(builder) {
            val n = substitution.newVariable()
            congruent(lhd.second.substitute(lhd.first, id(n)), rhd.second.substitute(rhd.first, id(n)))
        }

    private fun Ast.Term<C>.reduce(): Ast.Term<C> = when (this) {
        is Ast.Term.Apply -> when (val abstraction = this.abstraction) {
            is Ast.Term.Lambda -> abstraction.body.substitute(abstraction.n, this.argument)
            else -> this
        }
        else -> this
    }

}