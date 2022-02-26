package io.smallibs.lang.nethra.ast

import io.smallibs.lang.nethra.ast.impl.BuilderImpl

interface Builder<C> {
    fun type(level: Int = 0): Ast.Term.Type<C>
    fun data(value: String, type: Ast.Term<C>): Ast.Term.Data<C>
    fun id(value: String): Ast.Term.Id<C>

    fun int(value: Int): Ast.Term.Lit<C>
    fun char(value: Char): Ast.Term.Lit<C>
    fun string(value: String): Ast.Term.Lit<C>

    infix fun Ast.Term<C>.arrow(body: Ast.Term<C>): Ast.Term.Pi<C> = pi("_", this, body)
    fun pi(n: String, bound: Ast.Term<C>, body: Ast.Term<C>, i: Boolean = false): Ast.Term.Pi<C>
    fun lambda(n: String, body: Ast.Term<C>, i: Boolean = false): Ast.Term.Lambda<C>
    fun apply(abstraction: Ast.Term<C>, argument: Ast.Term<C>, i: Boolean = false): Ast.Term.Apply<C>

    infix fun Ast.Term<C>.and(body: Ast.Term<C>): Ast.Term.Sigma<C> = sigma("_", this, body)
    fun sigma(n: String, bound: Ast.Term<C>, body: Ast.Term<C>): Ast.Term.Sigma<C>
    fun pair(lhd: Ast.Term<C>, rhd: Ast.Term<C>): Ast.Term.Couple<C>
    fun fst(e: Ast.Term<C>): Ast.Term.Fst<C>
    fun snd(e: Ast.Term<C>): Ast.Term.Snd<C>

    fun or(lhd: Ast.Term<C>, rhd: Ast.Term<C>): Ast.Term.Disjunction<C>
    fun inl(e: Ast.Term<C>): Ast.Term.Inl<C>
    fun inr(e: Ast.Term<C>): Ast.Term.Inr<C>
    fun case(e: Ast.Term<C>, l: Ast.Term<C>, r: Ast.Term<C>): Ast.Term.Case<C>

    fun rec(self: String, value: Ast.Term<C>): Ast.Term.Rec<C>
    fun fold(e: Ast.Term<C>): Ast.Term.Fold<C>
    fun unfold(e: Ast.Term<C>): Ast.Term.Unfold<C>

    fun inhabit(term: Ast.Term<C>, type: Ast.Term<C>): Ast.Term.Inhabit<C>

    fun hole(name:String): Ast.Term.Hole<C>

    companion object {
        operator fun <C> invoke(): Builder<C> = BuilderImpl()
    }
}