package io.smallibs.lang.nethra.ast

object Ast {

    sealed class Binding<C>(open var context: C? = null) {
        data class Signature<C>(val name: String, val value: Term<C>) : Binding<C>()
        data class Definition<C>(val name: String, val value: Term<C>) : Binding<C>()
    }

    sealed class Term<C>(open var context: C? = null) {

        sealed interface Literal {
            data class IntLit(val value: Int) : Literal
            data class CharLit(val value: Char) : Literal
            data class StringLit(val value: String) : Literal
        }

        data class Type<C>(val level: Int = 0) : Term<C>()
        data class Id<C>(val value: String) : Term<C>()

        data class Lit<C>(val literal: Literal) : Term<C>()

        data class Pi<C>(val n: String, val bound: Term<C>, val body: Term<C>, val implicit: Boolean = false) :
            Term<C>()

        data class Lambda<C>(val n: String, val body: Term<C>, val implicit: Boolean = false) : Term<C>()
        data class Apply<C>(val abstraction: Term<C>, val argument: Term<C>, val implicit: Boolean = false) : Term<C>()

        data class Sigma<C>(val n: String, val bound: Term<C>, val body: Term<C>) : Term<C>()
        data class Couple<C>(val lhd: Term<C>, val rhd: Term<C>) : Term<C>()
        data class Fst<C>(val term: Term<C>) : Term<C>()
        data class Snd<C>(val term: Term<C>) : Term<C>()

        data class Disjunction<C>(val lhd: Term<C>, val rhd: Term<C>) : Term<C>()
        data class Inl<C>(val term: Term<C>) : Term<C>()
        data class Inr<C>(val term: Term<C>) : Term<C>()
        data class Case<C>(val term: Term<C>, val left: Term<C>, val right: Term<C>) : Term<C>()

        data class Rec<C>(val self: String, val body: Term<C>) : Term<C>()
        data class Fold<C>(val term: Term<C>) : Term<C>()
        data class Unfold<C>(val term: Term<C>) : Term<C>()

        data class Inhabit<C>(val term: Term<C>, val type: Term<C>) : Term<C>()

        data class Hole<C>(val value: String, var term: Term<C>? = null) : Term<C>()
    }

}

