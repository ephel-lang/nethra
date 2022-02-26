package io.smallibs.lang.nethra.ast.impl

import io.smallibs.lang.nethra.ast.Ast
import io.smallibs.lang.nethra.ast.Builder
import io.smallibs.lang.nethra.ast.Substitution
import io.smallibs.lang.nethra.ast.Visitor

class SubstitutionImpl<C>(
    private val constructor: Builder<C> = Builder(),
) : Substitution<C>, Visitor<C, Pair<String, Ast.Term<C>>, Ast.Term<C>>, Builder<C> by constructor {

    // UGLY !
    companion object {
        private var index: Int = 0
    }

    override fun newVariable(): String = index.let { "\$${index++}" }

    override fun Ast.Term<C>.substitute(name: String, term: Ast.Term<C>) = this.run(name to term)

    /**
     * Interpret implementation
     */

    override fun Ast.Term.Type<C>.run(i: Pair<String, Ast.Term<C>>) = this

    override fun Ast.Term.Lit<C>.run(i: Pair<String, Ast.Term<C>>) = this

    override fun Ast.Term.Id<C>.run(i: Pair<String, Ast.Term<C>>) = if (this.value == i.first) i.second else this

    override fun Ast.Term.Pi<C>.run(i: Pair<String, Ast.Term<C>>) =
        if (n == i.first) pi(n, bound.run(i), body, implicit) else pi(n, bound.run(i), body.run(i), implicit)

    override fun Ast.Term.Lambda<C>.run(i: Pair<String, Ast.Term<C>>) =
        if (n == i.first) this@run else lambda(n, body.run(i), implicit)

    override fun Ast.Term.Apply<C>.run(i: Pair<String, Ast.Term<C>>) =
        apply(abstraction.run(i), argument.run(i), implicit)

    override fun Ast.Term.Sigma<C>.run(i: Pair<String, Ast.Term<C>>) =
        if (n == i.first) sigma(n, bound, body.run(i)) else sigma(n, bound.run(i), body.run(i))

    override fun Ast.Term.Couple<C>.run(i: Pair<String, Ast.Term<C>>) = pair(lhd.run(i), rhd.run(i))

    override fun Ast.Term.Fst<C>.run(i: Pair<String, Ast.Term<C>>) = fst(term.run(i))

    override fun Ast.Term.Snd<C>.run(i: Pair<String, Ast.Term<C>>) = snd(term.run(i))

    override fun Ast.Term.Disjunction<C>.run(i: Pair<String, Ast.Term<C>>) = or(lhd.run(i), rhd.run(i))

    override fun Ast.Term.Inl<C>.run(i: Pair<String, Ast.Term<C>>) = inl(term.run(i))

    override fun Ast.Term.Inr<C>.run(i: Pair<String, Ast.Term<C>>) = inr(term.run(i))

    override fun Ast.Term.Case<C>.run(i: Pair<String, Ast.Term<C>>): Ast.Term<C> =
        case(term.run(i), left.run(i), right.run(i))

    override fun Ast.Term.Rec<C>.run(i: Pair<String, Ast.Term<C>>) =
        if (self == i.first) this@run else rec(self, body.run(i))

    override fun Ast.Term.Fold<C>.run(i: Pair<String, Ast.Term<C>>) = fold(term.run(i))

    override fun Ast.Term.Unfold<C>.run(i: Pair<String, Ast.Term<C>>) = unfold(term.run(i))

    override fun Ast.Term.Inhabit<C>.run(i: Pair<String, Ast.Term<C>>) = inhabit(term.run(i), type.run(i))

    override fun Ast.Term.Hole<C>.run(i: Pair<String, Ast.Term<C>>): Ast.Term<C> = this
}