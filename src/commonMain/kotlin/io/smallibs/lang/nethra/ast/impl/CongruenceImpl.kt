package io.smallibs.lang.nethra.ast.impl

import io.smallibs.lang.nethra.ast.Ast
import io.smallibs.lang.nethra.ast.Ast.Term.Apply
import io.smallibs.lang.nethra.ast.Ast.Term.Couple
import io.smallibs.lang.nethra.ast.Ast.Term.Fold
import io.smallibs.lang.nethra.ast.Ast.Term.Fst
import io.smallibs.lang.nethra.ast.Ast.Term.Hole
import io.smallibs.lang.nethra.ast.Ast.Term.Id
import io.smallibs.lang.nethra.ast.Ast.Term.Inl
import io.smallibs.lang.nethra.ast.Ast.Term.Inr
import io.smallibs.lang.nethra.ast.Ast.Term.Lambda
import io.smallibs.lang.nethra.ast.Ast.Term.Lit
import io.smallibs.lang.nethra.ast.Ast.Term.Pi
import io.smallibs.lang.nethra.ast.Ast.Term.Sigma
import io.smallibs.lang.nethra.ast.Ast.Term.Snd
import io.smallibs.lang.nethra.ast.Ast.Term.Type
import io.smallibs.lang.nethra.ast.Ast.Term.Unfold
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
            //println("[?] ${lhd.prettyPrint()} ≅ ${rhd.prettyPrint()} / ?").let {
            Unit.let {
                val r = if (lhd.isHole() || !rhd.isHole()) lhd.run(this to rhd)
                else rhd.run(this to lhd)
                // println("[?] ${lhd.prettyPrint()} ≅ ${rhd.prettyPrint()} / $r")
                r
            }
        }

    /**
     * Visitor implementation
     */

    override fun Type<C>.run(i: Pair<Bindings<C>, Ast.Term<C>>) = when (val t = i.second) {
        is Type -> this.level == t.level
        else -> i.first.fallback(this, i.second)
    }

    override fun Id<C>.run(i: Pair<Bindings<C>, Ast.Term<C>>) = when (val t = i.second) {
        is Id -> this.value == t.value
        else -> i.first.fallback(this, i.second)
    }

    override fun Lit<C>.run(i: Pair<Bindings<C>, Ast.Term<C>>) = when (val t = i.second) {
        is Lit -> this.literal == t.literal
        else -> i.first.fallback(this, i.second)
    }

    override fun Pi<C>.run(i: Pair<Bindings<C>, Ast.Term<C>>) = when (val t = i.second) {
        is Pi -> i.first.congruent(this.bound, t.bound) && i.first.withBindingEquals(this.n to this.body, t.n to t.body)
        else -> i.first.fallback(this, i.second)
    }

    override fun Lambda<C>.run(i: Pair<Bindings<C>, Ast.Term<C>>) = when (val t = i.second) {
        is Lambda -> i.first.withBindingEquals(this.n to this.body, t.n to t.body)
        else -> i.first.fallback(this, i.second)
    }

    override fun Apply<C>.run(i: Pair<Bindings<C>, Ast.Term<C>>) = when (val t = i.second) {
        is Apply -> i.first.congruent(this.abstraction, t.abstraction) && i.first.congruent(this.argument, t.argument)
        else -> i.first.fallback(this, i.second)
    }

    override fun Sigma<C>.run(i: Pair<Bindings<C>, Ast.Term<C>>) = when (val t = i.second) {
        is Sigma -> i.first.congruent(bound, t.bound) && i.first.withBindingEquals(this.n to this.body, t.n to t.body)
        else -> i.first.fallback(this, i.second)
    }

    override fun Couple<C>.run(i: Pair<Bindings<C>, Ast.Term<C>>) = when (val t = i.second) {
        is Couple -> i.first.congruent(this.lhd, t.lhd) && i.first.congruent(this.rhd, t.rhd)
        else -> i.first.fallback(this, i.second)
    }

    override fun Fst<C>.run(i: Pair<Bindings<C>, Ast.Term<C>>) = when (val t = i.second) {
        is Fst -> i.first.congruent(term, t.term)
        else -> i.first.fallback(this, i.second)
    }

    override fun Snd<C>.run(i: Pair<Bindings<C>, Ast.Term<C>>) = when (val t = i.second) {
        is Snd -> i.first.congruent(term, t.term)
        else -> i.first.fallback(this, i.second)
    }

    override fun Ast.Term.Disjunction<C>.run(i: Pair<Bindings<C>, Ast.Term<C>>) = when (val t = i.second) {
        is Ast.Term.Disjunction -> i.first.congruent(this.lhd, t.lhd) && i.first.congruent(this.rhd, t.rhd)
        else -> i.first.fallback(this, i.second)
    }

    override fun Inl<C>.run(i: Pair<Bindings<C>, Ast.Term<C>>) = when (val t = i.second) {
        is Inl -> i.first.congruent(term, t.term)
        else -> i.first.fallback(this, i.second)
    }

    override fun Inr<C>.run(i: Pair<Bindings<C>, Ast.Term<C>>) = when (val t = i.second) {
        is Inr -> i.first.congruent(term, t.term)
        else -> i.first.fallback(this, i.second)
    }

    override fun Ast.Term.Case<C>.run(i: Pair<Bindings<C>, Ast.Term<C>>) = when (val t = i.second) {
        is Ast.Term.Case -> i.first.congruent(term, t.term) && i.first.congruent(left, t.left) && i.first.congruent(
            right,
            t.right)
        else -> i.first.fallback(this, i.second)
    }

    override fun Ast.Term.Rec<C>.run(i: Pair<Bindings<C>, Ast.Term<C>>) = when (val t = i.second) {
        is Ast.Term.Rec -> i.first.withBindingEquals(this.self to this.body, t.self to t.body)
        else -> i.first.fallback(this, i.second)
    }

    override fun Fold<C>.run(i: Pair<Bindings<C>, Ast.Term<C>>) = when (val t = i.second) {
        is Fold -> i.first.congruent(term, t.term)
        else -> i.first.fallback(this, i.second)
    }

    override fun Unfold<C>.run(i: Pair<Bindings<C>, Ast.Term<C>>) = when (val t = i.second) {
        is Unfold -> i.first.congruent(term, t.term)
        else -> i.first.fallback(this, i.second)
    }

    override fun Hole<C>.run(i: Pair<Bindings<C>, Ast.Term<C>>) = when (ref.value) {
        null -> {
            when (val t = i.second) {
                is Hole -> if (value == t.value) {
                    true
                } else {
                    ref.value = i.second
                    true
                }
                else -> {
                    ref.value = i.second
                    true
                }
            }
        }
        else -> i.first.congruent(ref.value!!, i.second)
    }

    /**
     * Private behaviors
     */

    private fun Bindings<C>.fallback(lhd: Ast.Term<C>, rhd: Ast.Term<C>) =
        when (lhd) {
            is Pi -> lhd.implicit && congruent(lhd.body.substitute(lhd.n to builder.hole(newVariable())), rhd)
            else -> when (rhd) {
                is Pi -> rhd.implicit && congruent(lhd, rhd.body.substitute(rhd.n to builder.hole(newVariable())))
                else -> false
            }
        }

    private fun Bindings<C>.withBindingEquals(lhd: Pair<String, Ast.Term<C>>, rhd: Pair<String, Ast.Term<C>>): Boolean =
        with(builder) {
            val n = substitution.newVariable()
            congruent(lhd.second.substitute(lhd.first to id(n)), rhd.second.substitute(rhd.first to id(n)))
        }

    private fun Ast.Term<C>.isHole(): Boolean = this is Hole<C> && this.ref.value == null
}