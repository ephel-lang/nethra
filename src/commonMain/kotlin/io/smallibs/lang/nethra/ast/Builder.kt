package io.smallibs.lang.nethra.ast

import io.smallibs.lang.nethra.ast.impl.BuilderImpl

interface Builder<C> {
    fun type(level: Int = 0): Term.Type<C>
    fun data(value: String, type: Term<C>): Term.Data<C>
    fun id(value: String): Term.Id<C>

    fun int(value: Int): Term.Lit<C>
    fun char(value: Char): Term.Lit<C>

    infix fun Term<C>.arrow(body: Term<C>): Term.Pi<C> = pi("_", this, body)
    fun pi(n: String, bound: Term<C>, body: Term<C>, i: Boolean = false): Term.Pi<C>
    fun lambda(n: String, body: Term<C>, i: Boolean = false): Term.Lambda<C>
    fun apply(abstraction: Term<C>, argument: Term<C>, i: Boolean = false): Term.Apply<C>

    infix fun Term<C>.and(body: Term<C>): Term.Sigma<C> = sigma("_", this, body)
    fun sigma(n: String, bound: Term<C>, body: Term<C>): Term.Sigma<C>
    fun pair(lhd: Term<C>, rhd: Term<C>): Term.Couple<C>
    fun fst(e: Term<C>): Term.Fst<C>
    fun snd(e: Term<C>): Term.Snd<C>

    fun or(lhd: Term<C>, rhd: Term<C>): Term.Disjunction<C>
    fun inl(e: Term<C>): Term.Inl<C>
    fun inr(e: Term<C>): Term.Inr<C>
    fun case(e: Term<C>, l: Term<C>, r: Term<C>): Term.Case<C>

    fun rec(self: String, value: Term<C>): Term.Rec<C>
    fun fold(e: Term<C>, t: Term.Rec<C>): Term.Fold<C>
    fun unfold(e: Term<C>, t: Term.Rec<C>): Term.Unfold<C>

    fun inhabit(term: Term<C>, type: Term<C>): Term.Inhabit<C>

    fun hole(name:String): Term.Hole<C>

    companion object {
        operator fun <C> invoke(): Builder<C> = BuilderImpl()
    }
}