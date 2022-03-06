package io.smallibs.lang.nethra.ast

object Ast {

    sealed class Binding<C>(open var context: C? = null) {
        data class Signature<C>(val name: String, val value: Term<C>) : Binding<C>()
        data class Definition<C>(val name: String, val value: Term<C>) : Binding<C>()

        fun set(context: C?): Binding<C> {
            this.context = context
            return this
        }
    }

    sealed class Term<C>(open val context: C? = null) {

        abstract fun set(context: C?): Term<C>

        sealed interface Literal {
            data class IntLit(val value: Int) : Literal
            data class CharLit(val value: Char) : Literal
            data class StringLit(val value: String) : Literal
        }

        data class Type<C>(val level: Int = 0, override val context: C? = null) : Term<C>(context) {
            override fun set(context: C?): Term<C> = Type(level, context)
        }

        data class Id<C>(val value: String, override val context: C? = null) : Term<C>(context) {
            override fun set(context: C?): Term<C> = Id(value, context)
        }

        data class Lit<C>(val literal: Literal, override val context: C? = null) : Term<C>(context) {
            override fun set(context: C?): Term<C> = Lit(literal, context)
        }

        data class Pi<C>(
            val n: String,
            val bound: Term<C>,
            val body: Term<C>,
            val implicit: Boolean = false,
            override val context: C? = null,
        ) :
            Term<C>(context) {
            override fun set(context: C?): Term<C> = Pi(n, bound, body, implicit, context)
        }

        data class Lambda<C>(
            val n: String,
            val body: Term<C>,
            val implicit: Boolean = false,
            override val context: C? = null,
        ) : Term<C>(context) {
            override fun set(context: C?): Term<C> = Lambda(n, body, implicit, context)
        }

        data class Apply<C>(
            val abstraction: Term<C>,
            val argument: Term<C>,
            val implicit: Boolean = false,
            override val context: C? = null,
        ) : Term<C>(context) {
            override fun set(context: C?): Term<C> = Apply(abstraction, argument, implicit, context)
        }

        data class Sigma<C>(val n: String, val bound: Term<C>, val body: Term<C>, override val context: C? = null) :
            Term<C>(context) {
            override fun set(context: C?): Term<C> = Sigma(n, bound, body, context)
        }

        data class Couple<C>(val lhd: Term<C>, val rhd: Term<C>, override val context: C? = null) : Term<C>(context) {
            override fun set(context: C?): Term<C> = Couple(lhd, rhd, context)
        }

        data class Fst<C>(val term: Term<C>, override val context: C? = null) : Term<C>(context) {
            override fun set(context: C?): Term<C> = Fst(term, context)
        }

        data class Snd<C>(val term: Term<C>, override val context: C? = null) : Term<C>(context) {
            override fun set(context: C?): Term<C> = Snd(term, context)
        }

        data class Disjunction<C>(val lhd: Term<C>, val rhd: Term<C>, override val context: C? = null) :
            Term<C>(context) {
            override fun set(context: C?): Term<C> = Disjunction(lhd, rhd, context)
        }

        data class Inl<C>(val term: Term<C>, override val context: C? = null) : Term<C>(context) {
            override fun set(context: C?): Term<C> = Inl(term, context)
        }

        data class Inr<C>(val term: Term<C>, override val context: C? = null) : Term<C>(context) {
            override fun set(context: C?): Term<C> = Inr(term, context)
        }

        data class Case<C>(val term: Term<C>, val left: Term<C>, val right: Term<C>, override val context: C? = null) :
            Term<C>(context) {
            override fun set(context: C?): Term<C> = Case(term, left, right, context)
        }

        data class Rec<C>(val self: String, val body: Term<C>, override val context: C? = null) : Term<C>(context) {
            override fun set(context: C?): Term<C> = Rec(self, body, context)
        }

        data class Fold<C>(val term: Term<C>, override val context: C? = null) : Term<C>(context) {
            override fun set(context: C?): Term<C> = Fold(term, context)
        }

        data class Unfold<C>(val term: Term<C>, override val context: C? = null) : Term<C>(context) {
            override fun set(context: C?): Term<C> = Unfold(term, context)
        }

        data class Ref<A>(var value: A? = null)

        data class Hole<C>(val value: String, val ref: Ref<Term<C>> = Ref(), override val context: C? = null) :
            Term<C>(context) {
            override fun set(context: C?): Term<C> = Hole(value, ref, context)
        }

        companion object {
            const val ANON = "_"
        }
    }

}

