package io.smallibs.lang.nethra.ast.impl

import io.smallibs.lang.nethra.ast.Builder
import io.smallibs.lang.nethra.ast.Term

class BuilderImpl<C> : Builder<C> {
    override fun type(level: Int) = Term.Type<C>(level)
    override fun data(value: String, type: Term<C>) = Term.Data(value, type)
    override fun id(value: String) = Term.Id<C>(value)

    override fun int(value: Int) = Term.Lit<C>(Term.Literal.IntLit(value))
    override fun char(value: Char) = Term.Lit<C>(Term.Literal.CharLit(value))

    override fun pi(n: String, bound: Term<C>, body: Term<C>, i: Boolean) = Term.Pi(n, bound, body, i)
    override fun lambda(n: String, body: Term<C>, i: Boolean) = Term.Lambda(n, body, i)
    override fun apply(abstraction: Term<C>, argument: Term<C>, i: Boolean) = Term.Apply(abstraction, argument, i)

    override fun sigma(n: String, bound: Term<C>, body: Term<C>) = Term.Sigma(n, bound, body)
    override fun pair(lhd: Term<C>, rhd: Term<C>) = Term.Couple(lhd, rhd)
    override fun fst(e: Term<C>) = Term.Fst(e)
    override fun snd(e: Term<C>) = Term.Snd(e)

    override fun or(lhd: Term<C>, rhd: Term<C>) = Term.Disjunction(lhd, rhd)
    override fun inl(e: Term<C>) = Term.Inl(e)
    override fun inr(e: Term<C>) = Term.Inr(e)
    override fun case(e: Term<C>, l: Term<C>, r: Term<C>): Term.Case<C> = Term.Case(e, l, r)

    override fun rec(self: String, value: Term<C>) = Term.Rec(self, value)
    override fun fold(e: Term<C>, t: Term.Rec<C>) = Term.Fold(t, e)
    override fun unfold(e: Term<C>, t: Term.Rec<C>) = Term.Unfold(t, e)

    override fun inhabit(term: Term<C>, type: Term<C>) = Term.Inhabit(term, type)

    override fun hole(name: String): Term.Hole<C> = Term.Hole(name)
}