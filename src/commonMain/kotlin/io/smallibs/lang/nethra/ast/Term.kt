package io.smallibs.lang.nethra.ast

/*
    n in Ident
    i in Int
    c in Char

    e ::=
        Type_i                -- Type at level i
        n                     -- Variable
        C(i:e)                -- Constructor                            ???

        i                     -- Integer literal
        c                     -- Character literal

        Π(n:e).e   Π{n:e}.e   -- Dependant function type
        λ(n).e     λ{n}.e     -- Function
        e e        e {e}      -- Application

        Σ(n:e).e              -- Dependant pair type
        e , e                 -- Pair
        fst e                 -- Left projection
        snd e                 -- Right Projection

        e + e                 -- Disjunction
        inl e                 -- Left injection
        inr e                 -- Right injection
        case e e e            -- Catamorphism

        rec(n).e              -- Recursion
        fold (rec(n).e) e     -- Fold recursive type
        unfold (rec(n).e) e   -- Unfold recursive type

        e ∈ e                 -- Term inhabits Type

        ?n                    -- Hole for inference
*/

sealed class Term<C>(open val context: C? = null) {

    sealed interface Literal {
        data class IntLit(val value: Int) : Literal
        data class CharLit(val value: Char) : Literal
    }

    data class Type<C>(val level: Int = 0) : Term<C>()
    data class Data<C>(val value: String, val type: Term<C>) : Term<C>()
    data class Id<C>(val value: String) : Term<C>()

    data class Lit<C>(val literal: Literal) : Term<C>()

    data class Pi<C>(val n: String, val bound: Term<C>, val body: Term<C>, val implicit: Boolean = false) : Term<C>()
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
    data class Fold<C>(val type: Rec<C>, val term: Term<C>) : Term<C>()
    data class Unfold<C>(val type: Rec<C>, val term: Term<C>) : Term<C>()

    data class Inhabit<C>(val term: Term<C>, val type: Term<C>) : Term<C>()

    data class Hole<C>(val value: String, var term: Term<C>? = null) : Term<C>()
}
