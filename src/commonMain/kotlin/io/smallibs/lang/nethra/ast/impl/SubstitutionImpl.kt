package io.smallibs.lang.nethra.ast.impl

import io.smallibs.lang.nethra.ast.Builder
import io.smallibs.lang.nethra.ast.Interpret
import io.smallibs.lang.nethra.ast.Substitution
import io.smallibs.lang.nethra.ast.Term

class SubstitutionImpl<C>(
    private var index: Int = 0,
    private val constructor: Builder<C> = Builder(),
) : Substitution<C>, Interpret<C, Pair<String, Term<C>>, Term<C>>, Builder<C> by constructor {

    override fun newVariable(): String = this.index.let { "\$${index++}" }

    override fun Term<C>.substitute(name: String, term: Term<C>) = this.run(name to term)

    /**
     * Interpret implementation
     */

    override fun Term.Type<C>.run(i: Pair<String, Term<C>>) = this

    override fun Term.Data<C>.run(i: Pair<String, Term<C>>) = this

    override fun Term.Lit<C>.run(i: Pair<String, Term<C>>) = this

    override fun Term.Id<C>.run(i: Pair<String, Term<C>>) = if (this.value == i.first) i.second else this

    override fun Term.Pi<C>.run(i: Pair<String, Term<C>>) =
        if (n == i.first) pi(n, bound.run(i), body, implicit) else pi(n, bound.run(i), body.run(i), implicit)

    override fun Term.Lambda<C>.run(i: Pair<String, Term<C>>) =
        if (n == i.first) this@run else lambda(n, body.run(i), implicit)

    override fun Term.Apply<C>.run(i: Pair<String, Term<C>>) = apply(abstraction.run(i), argument.run(i), implicit)

    override fun Term.Sigma<C>.run(i: Pair<String, Term<C>>) =
        if (n == i.first) sigma(n, bound, body.run(i)) else sigma(n, bound.run(i), body.run(i))

    override fun Term.Couple<C>.run(i: Pair<String, Term<C>>) = pair(lhd.run(i), rhd.run(i))

    override fun Term.Fst<C>.run(i: Pair<String, Term<C>>) = fst(term.run(i))

    override fun Term.Snd<C>.run(i: Pair<String, Term<C>>) = snd(term.run(i))

    override fun Term.Disjunction<C>.run(i: Pair<String, Term<C>>) = or(lhd.run(i), rhd.run(i))

    override fun Term.Inl<C>.run(i: Pair<String, Term<C>>) = inl(term.run(i))

    override fun Term.Inr<C>.run(i: Pair<String, Term<C>>) = inr(term.run(i))

    override fun Term.Case<C>.run(i: Pair<String, Term<C>>): Term<C> = case(term.run(i), left.run(i), right.run(i))

    override fun Term.Rec<C>.run(i: Pair<String, Term<C>>) = if (self == i.first) this@run else rec(self, body.run(i))

    override fun Term.Fold<C>.run(i: Pair<String, Term<C>>) = fold(term.run(i), type)

    override fun Term.Unfold<C>.run(i: Pair<String, Term<C>>) = unfold(term.run(i), type)

    override fun Term.Inhabit<C>.run(i: Pair<String, Term<C>>) = inhabit(term.run(i), type.run(i))

    override fun Term.Hole<C>.run(i: Pair<String, Term<C>>): Term<C> = this
}