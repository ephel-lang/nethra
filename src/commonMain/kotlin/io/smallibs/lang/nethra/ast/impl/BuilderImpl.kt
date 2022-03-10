package io.smallibs.lang.nethra.ast.impl

import io.smallibs.lang.nethra.ast.Ast
import io.smallibs.lang.nethra.ast.Builder

class BuilderImpl<C> : Builder<C> {
    override fun type(level: Int) = Ast.Term.Type<C>(level)
    override fun id(value: String, external: String?) = Ast.Term.Id<C>(value, external)

    override fun int(value: Int) = Ast.Term.Lit<C>(Ast.Term.Literal.IntLit(value))
    override fun char(value: Char) = Ast.Term.Lit<C>(Ast.Term.Literal.CharLit(value))
    override fun string(value: String) = Ast.Term.Lit<C>(Ast.Term.Literal.StringLit(value))

    override fun pi(n: Ast.Term.Id<C>, bound: Ast.Term<C>, body: Ast.Term<C>, i: Boolean) = Ast.Term.Pi(n, bound, body, i)
    override fun lambda(n: Ast.Term.Id<C>, body: Ast.Term<C>, i: Boolean) = Ast.Term.Lambda(n, body, i)
    override fun apply(abstraction: Ast.Term<C>, argument: Ast.Term<C>, i: Boolean) =
        Ast.Term.Apply(abstraction, argument, i)

    override fun sigma(n: Ast.Term.Id<C>, bound: Ast.Term<C>, body: Ast.Term<C>) = Ast.Term.Sigma(n, bound, body)
    override fun pair(lhd: Ast.Term<C>, rhd: Ast.Term<C>) = Ast.Term.Couple(lhd, rhd)
    override fun fst(e: Ast.Term<C>) = Ast.Term.Fst(e)
    override fun snd(e: Ast.Term<C>) = Ast.Term.Snd(e)

    override fun or(lhd: Ast.Term<C>, rhd: Ast.Term<C>) = Ast.Term.Disjunction(lhd, rhd)
    override fun inl(e: Ast.Term<C>) = Ast.Term.Inl(e)
    override fun inr(e: Ast.Term<C>) = Ast.Term.Inr(e)
    override fun case(e: Ast.Term<C>, l: Ast.Term<C>, r: Ast.Term<C>): Ast.Term.Case<C> = Ast.Term.Case(e, l, r)

    override fun rec(self: Ast.Term.Id<C>, value: Ast.Term<C>) = Ast.Term.Rec(self, value)
    override fun fold(e: Ast.Term<C>) = Ast.Term.Fold(e)
    override fun unfold(e: Ast.Term<C>) = Ast.Term.Unfold(e)

    override fun hole(name: String, external: String?): Ast.Term.Hole<C> = Ast.Term.Hole(name, external)
}