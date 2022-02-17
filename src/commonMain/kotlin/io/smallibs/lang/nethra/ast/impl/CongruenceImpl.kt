package io.smallibs.lang.nethra.ast.impl

import io.smallibs.lang.nethra.ast.Builder
import io.smallibs.lang.nethra.ast.Congruence
import io.smallibs.lang.nethra.ast.Interpret
import io.smallibs.lang.nethra.ast.Printer
import io.smallibs.lang.nethra.ast.Substitution
import io.smallibs.lang.nethra.ast.Term
import io.smallibs.lang.nethra.ast.Term.Data
import io.smallibs.lang.nethra.ast.Term.Id
import io.smallibs.lang.nethra.ast.Term.Type

//
// Stupid version based on strict equality for the moment
//

class CongruenceImpl<C>(
    private val substitution: Substitution<C> = Substitution(),
    private val builder: Builder<C> = Builder(),
    private val printer: Printer<C> = Printer(),
) : Interpret<C, Term<C>, Boolean>, Congruence<C>, Printer<C> by printer, Substitution<C> by substitution {

    override infix fun Term<C>.compatibleWith(i: Term<C>) =
        println("[≡] ${this.prettyPrint()} == ${i.prettyPrint()} / ?").let {
            val r = if (this.isHole() || !i.isHole()) this.run(i) else i.run(this)
            println("[≡] ${this.prettyPrint()} == ${i.prettyPrint()} / $r")
            r
        }

    private fun Term<C>.isHole(): Boolean = this is Term.Hole<C>

    /**
     * Interpret implementation
     */

    override fun Type<C>.run(i: Term<C>) = this == i

    override fun Data<C>.run(i: Term<C>) = this == i
    override fun Id<C>.run(i: Term<C>) = this == i

    override fun Term.Lit<C>.run(i: Term<C>) = this == i

    override fun Term.Pi<C>.run(i: Term<C>) = when (i) {
        is Term.Pi -> withBindingEquals(this.n to this.body, i.n to i.body)
        else -> false
    }

    override fun Term.Lambda<C>.run(i: Term<C>) = when (i) {
        is Term.Lambda -> withBindingEquals(this.n to this.body, i.n to i.body)
        else -> false
    }

    override fun Term.Apply<C>.run(i: Term<C>) = when (i) {
        is Term.Apply -> this.abstraction compatibleWith i.abstraction && this.argument compatibleWith i.argument
        else -> false
    }

    override fun Term.Sigma<C>.run(i: Term<C>) = when (i) {
        is Term.Sigma -> withBindingEquals(this.n to this.body, i.n to i.body)
        else -> false
    }

    override fun Term.Couple<C>.run(i: Term<C>) = when (i) {
        is Term.Couple -> this.lhd compatibleWith i.lhd && this.rhd compatibleWith i.rhd
        else -> false
    }

    override fun Term.Fst<C>.run(i: Term<C>) = this == i

    override fun Term.Snd<C>.run(i: Term<C>) = this == i

    override fun Term.Disjunction<C>.run(i: Term<C>) = when (i) {
        is Term.Disjunction -> this.lhd compatibleWith i.lhd && this.rhd compatibleWith i.rhd
        else -> false
    }

    override fun Term.Inl<C>.run(i: Term<C>) = this == i

    override fun Term.Inr<C>.run(i: Term<C>) = this == i

    override fun Term.Case<C>.run(i: Term<C>) = TODO()

    override fun Term.Rec<C>.run(i: Term<C>) = when (i) {
        is Term.Rec -> withBindingEquals(this.self to this.body, i.self to i.body)
        else -> false
    }

    override fun Term.Fold<C>.run(i: Term<C>) = this == i

    override fun Term.Unfold<C>.run(i: Term<C>) = this == i

    override fun Term.Inhabit<C>.run(i: Term<C>) = this == i

    override fun Term.Hole<C>.run(i: Term<C>) =
        when (term) {
            null -> {
                term = i
                true
            }
            else -> term!!.run(i)
        }

    /**
     * Private behaviors
     */

    private

    fun withBindingEquals(lhd: Pair<String, Term<C>>, rhd: Pair<String, Term<C>>) = with(builder) {
        val n = substitution.newVariable()
        lhd.second.substitute(lhd.first, id(n)) compatibleWith (rhd.second.substitute(rhd.first, id(n)))
    }

}