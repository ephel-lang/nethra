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

    override fun Ast.Term<C>.substitute(param: Pair<String, Ast.Term<C>>) = this.run(param).set(this.context)

    /**
     * Interpret implementation
     */

    override fun Ast.Term.Type<C>.run(i: Pair<String, Ast.Term<C>>) = this

    override fun Ast.Term.Lit<C>.run(i: Pair<String, Ast.Term<C>>) = this

    override fun Ast.Term.Id<C>.run(i: Pair<String, Ast.Term<C>>) =
        if (this.value == i.first) i.second.set(this.context) else this

    override fun Ast.Term.Pi<C>.run(i: Pair<String, Ast.Term<C>>) =
        if (n == i.first) pi(n, bound.substitute(i), body, implicit) else pi(n,
            bound.substitute(i),
            body.substitute(i),
            implicit)

    override fun Ast.Term.Lambda<C>.run(i: Pair<String, Ast.Term<C>>) =
        if (n == i.first) this@run else lambda(n, body.substitute(i), implicit)

    override fun Ast.Term.Apply<C>.run(i: Pair<String, Ast.Term<C>>) =
        apply(abstraction.substitute(i), argument.substitute(i), implicit)

    override fun Ast.Term.Sigma<C>.run(i: Pair<String, Ast.Term<C>>) =
        if (n == i.first) sigma(n, bound, body.substitute(i)) else sigma(n, bound.substitute(i), body.substitute(i))

    override fun Ast.Term.Couple<C>.run(i: Pair<String, Ast.Term<C>>) = pair(lhd.substitute(i), rhd.substitute(i))

    override fun Ast.Term.Fst<C>.run(i: Pair<String, Ast.Term<C>>) = fst(term.substitute(i))

    override fun Ast.Term.Snd<C>.run(i: Pair<String, Ast.Term<C>>) = snd(term.substitute(i))

    override fun Ast.Term.Disjunction<C>.run(i: Pair<String, Ast.Term<C>>) = or(lhd.substitute(i), rhd.substitute(i))

    override fun Ast.Term.Inl<C>.run(i: Pair<String, Ast.Term<C>>) = inl(term.substitute(i))

    override fun Ast.Term.Inr<C>.run(i: Pair<String, Ast.Term<C>>) = inr(term.substitute(i))

    override fun Ast.Term.Case<C>.run(i: Pair<String, Ast.Term<C>>): Ast.Term<C> =
        case(term.substitute(i), left.substitute(i), right.substitute(i))

    override fun Ast.Term.Rec<C>.run(i: Pair<String, Ast.Term<C>>) =
        if (self == i.first) this@run else rec(self, body.substitute(i))

    override fun Ast.Term.Fold<C>.run(i: Pair<String, Ast.Term<C>>) = fold(term.substitute(i))

    override fun Ast.Term.Unfold<C>.run(i: Pair<String, Ast.Term<C>>) = unfold(term.substitute(i))

    override fun Ast.Term.Inhabit<C>.run(i: Pair<String, Ast.Term<C>>) = inhabit(term.substitute(i), type.substitute(i))

    override fun Ast.Term.Hole<C>.run(i: Pair<String, Ast.Term<C>>): Ast.Term<C> = this

}