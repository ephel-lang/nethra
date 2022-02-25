package io.smallibs.lang.nethra.ast.impl

import io.smallibs.lang.nethra.ast.Ast
import io.smallibs.lang.nethra.ast.Ast.Term.Apply
import io.smallibs.lang.nethra.ast.Ast.Term.Data
import io.smallibs.lang.nethra.ast.Ast.Term.Hole
import io.smallibs.lang.nethra.ast.Ast.Term.Id
import io.smallibs.lang.nethra.ast.Ast.Term.Inl
import io.smallibs.lang.nethra.ast.Ast.Term.Inr
import io.smallibs.lang.nethra.ast.Ast.Term.Type
import io.smallibs.lang.nethra.ast.Builder
import io.smallibs.lang.nethra.ast.Congruence
import io.smallibs.lang.nethra.ast.Printer
import io.smallibs.lang.nethra.ast.Reducer
import io.smallibs.lang.nethra.ast.Substitution
import io.smallibs.lang.nethra.ast.Visitor
import io.smallibs.lang.nethra.stages.s03_Checker.internal.Bindings

class CongruenceImpl<C>(
    private val substitution: Substitution<C> = Substitution(),
    private val builder: Builder<C> = Builder(),
    private val printer: Printer<C> = Printer(),
    private val reducer: Reducer<C> = Reducer(),
) : Visitor<C, Pair<Bindings<C>, Ast.Term<C>>, Boolean>, Congruence<C>, Printer<C> by printer,
    Substitution<C> by substitution, Reducer<C> by reducer {

    override fun Bindings<C>.congruent(lhd: Ast.Term<C>, rhd: Ast.Term<C>) =
        (reduce(lhd) to reduce(rhd)).let { (lhd, rhd) ->
            println("[?] ${lhd.prettyPrint()} ≅ ${rhd.prettyPrint()} / ?").let {
                val r = if (lhd.isHole() || !rhd.isHole()) lhd.run(this to rhd)
                else rhd.run(this to lhd)
                println("[?] ${lhd.prettyPrint()} ≅ ${rhd.prettyPrint()} / $r")
                r
            }
        }

    /**
     * Interpret implementation
     */

    override fun Type<C>.run(i: Pair<Bindings<C>, Ast.Term<C>>) = this == i.second
    override fun Data<C>.run(i: Pair<Bindings<C>, Ast.Term<C>>) = this == i.second
    override fun Id<C>.run(i: Pair<Bindings<C>, Ast.Term<C>>) = this == i.second
    override fun Ast.Term.Lit<C>.run(i: Pair<Bindings<C>, Ast.Term<C>>) = this == i.second

    override fun Ast.Term.Pi<C>.run(i: Pair<Bindings<C>, Ast.Term<C>>) = when (val t = i.second) {
        is Ast.Term.Pi -> i.first.congruent(this.bound, t.bound) && i.first.withBindingEquals(this.n to this.body,
            t.n to t.body)
        else -> false
    }

    override fun Ast.Term.Lambda<C>.run(i: Pair<Bindings<C>, Ast.Term<C>>) = when (val t = i.second) {
        is Ast.Term.Lambda -> i.first.withBindingEquals(this.n to this.body, t.n to t.body)
        else -> false
    }

    override fun Apply<C>.run(i: Pair<Bindings<C>, Ast.Term<C>>) = when (val t = i.second) {
        is Apply -> i.first.congruent(this.abstraction, t.abstraction) && i.first.congruent(this.argument, t.argument)
        else -> false
    }

    override fun Ast.Term.Sigma<C>.run(i: Pair<Bindings<C>, Ast.Term<C>>) = when (val t = i.second) {
        is Ast.Term.Sigma -> i.first.withBindingEquals(this.n to this.body, t.n to t.body)
        else -> false
    }

    override fun Ast.Term.Couple<C>.run(i: Pair<Bindings<C>, Ast.Term<C>>) = when (val t = i.second) {
        is Ast.Term.Couple -> i.first.congruent(this.lhd, t.lhd) && i.first.congruent(this.rhd, t.rhd)
        else -> false
    }

    override fun Ast.Term.Fst<C>.run(i: Pair<Bindings<C>, Ast.Term<C>>) = this == i.second

    override fun Ast.Term.Snd<C>.run(i: Pair<Bindings<C>, Ast.Term<C>>) = this == i.second

    override fun Ast.Term.Disjunction<C>.run(i: Pair<Bindings<C>, Ast.Term<C>>) = when (val t = i.second) {
        is Ast.Term.Disjunction -> i.first.congruent(this.lhd, t.lhd) && i.first.congruent(this.rhd, t.rhd)
        else -> false
    }

    override fun Inl<C>.run(i: Pair<Bindings<C>, Ast.Term<C>>) = this == i

    override fun Inr<C>.run(i: Pair<Bindings<C>, Ast.Term<C>>) = this == i

    override fun Ast.Term.Case<C>.run(i: Pair<Bindings<C>, Ast.Term<C>>) = TODO()

    override fun Ast.Term.Rec<C>.run(i: Pair<Bindings<C>, Ast.Term<C>>) = when (val t = i.second) {
        is Ast.Term.Rec -> i.first.withBindingEquals(this.self to this.body, t.self to t.body)
        else -> false
    }

    override fun Ast.Term.Fold<C>.run(i: Pair<Bindings<C>, Ast.Term<C>>) = this == i.second

    override fun Ast.Term.Unfold<C>.run(i: Pair<Bindings<C>, Ast.Term<C>>) = this == i.second

    override fun Ast.Term.Inhabit<C>.run(i: Pair<Bindings<C>, Ast.Term<C>>) = this == i.second

    override fun Hole<C>.run(i: Pair<Bindings<C>, Ast.Term<C>>) = when (term) {
        null -> {
            term = i.second
            true
        }
        else -> term!!.run(i)
    }

    /**
     * Private behaviors
     */

    private fun Bindings<C>.withBindingEquals(lhd: Pair<String, Ast.Term<C>>, rhd: Pair<String, Ast.Term<C>>): Boolean =
        with(builder) {
            val n = substitution.newVariable()
            congruent(lhd.second.substitute(lhd.first, id(n)), rhd.second.substitute(rhd.first, id(n)))
        }

    private fun Ast.Term<C>.isHole(): Boolean = this is Hole<C>

}