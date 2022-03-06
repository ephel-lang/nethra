package io.smallibs.lang.nethra.ast.impl

import io.smallibs.lang.nethra.ast.Ast
import io.smallibs.lang.nethra.ast.Builder
import io.smallibs.lang.nethra.ast.Congruence
import io.smallibs.lang.nethra.ast.Replace
import io.smallibs.lang.nethra.ast.Visitor
import io.smallibs.lang.nethra.stages.s03_Checker.internal.Bindings

class ReplaceImpl<C>(
    private val congruence: Congruence<C> = Congruence(),
    private val constructor: Builder<C> = Builder(),
) : Replace<C>, Visitor<C, Pair<Ast.Term<C>, Ast.Term<C>>, Ast.Term<C>>,
    Congruence<C> by congruence,
    Builder<C> by constructor {

    override fun Ast.Term<C>.replace(param: Pair<Ast.Term<C>, Ast.Term<C>>) =
        if (Bindings<C>().congruent(this, param.first)) {
            param.second
        } else {
            this.run(param).set(this.context)
        }

    /**
     * Visitor implementation
     */

    override fun Ast.Term.Type<C>.run(i: Pair<Ast.Term<C>, Ast.Term<C>>) = this

    override fun Ast.Term.Lit<C>.run(i: Pair<Ast.Term<C>, Ast.Term<C>>) = this

    override fun Ast.Term.Id<C>.run(i: Pair<Ast.Term<C>, Ast.Term<C>>) =
        this

    override fun Ast.Term.Pi<C>.run(i: Pair<Ast.Term<C>, Ast.Term<C>>) =
        pi(n,
            bound.replace(i),
            body.replace(i),
            implicit)

    override fun Ast.Term.Lambda<C>.run(i: Pair<Ast.Term<C>, Ast.Term<C>>) =
        lambda(n, body.replace(i), implicit)

    override fun Ast.Term.Apply<C>.run(i: Pair<Ast.Term<C>, Ast.Term<C>>) =
        apply(abstraction.replace(i), argument.replace(i), implicit)

    override fun Ast.Term.Sigma<C>.run(i: Pair<Ast.Term<C>, Ast.Term<C>>) =
        sigma(n, bound.replace(i), body.replace(i))

    override fun Ast.Term.Couple<C>.run(i: Pair<Ast.Term<C>, Ast.Term<C>>) = pair(lhd.replace(i), rhd.replace(i))

    override fun Ast.Term.Fst<C>.run(i: Pair<Ast.Term<C>, Ast.Term<C>>) = fst(term.replace(i))

    override fun Ast.Term.Snd<C>.run(i: Pair<Ast.Term<C>, Ast.Term<C>>) = snd(term.replace(i))

    override fun Ast.Term.Disjunction<C>.run(i: Pair<Ast.Term<C>, Ast.Term<C>>) = or(lhd.replace(i), rhd.replace(i))

    override fun Ast.Term.Inl<C>.run(i: Pair<Ast.Term<C>, Ast.Term<C>>) = inl(term.replace(i))

    override fun Ast.Term.Inr<C>.run(i: Pair<Ast.Term<C>, Ast.Term<C>>) = inr(term.replace(i))

    override fun Ast.Term.Case<C>.run(i: Pair<Ast.Term<C>, Ast.Term<C>>): Ast.Term<C> =
        case(term.replace(i), left.replace(i), right.replace(i))

    override fun Ast.Term.Rec<C>.run(i: Pair<Ast.Term<C>, Ast.Term<C>>) =
        rec(self, body.replace(i))

    override fun Ast.Term.Fold<C>.run(i: Pair<Ast.Term<C>, Ast.Term<C>>) = fold(term.replace(i))

    override fun Ast.Term.Unfold<C>.run(i: Pair<Ast.Term<C>, Ast.Term<C>>) = unfold(term.replace(i))

    override fun Ast.Term.Hole<C>.run(i: Pair<Ast.Term<C>, Ast.Term<C>>): Ast.Term<C> = this

}