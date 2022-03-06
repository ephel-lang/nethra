package io.smallibs.lang.nethra.cst

import io.smallibs.parsec.parser.Region

object Cst {

    sealed interface Binding {
        val name: String
        val value: Localised<Term>

        data class Signature(override val name: String, override val value: Localised<Term>) : Binding
        data class Definition(override val name: String, override val value: Localised<Term>) : Binding

        fun pretty(atom: Boolean = false): String =
            when (this) {
                is Signature -> "sig $name : ${value.prettyTerm()}"
                is Definition -> "def $name = ${value.prettyTerm()}"
            }
    }

    sealed interface Term {
        data class Type(val level: Int) : Term
        data class IntLiteral(val value: Int) : Term
        data class CharLiteral(val value: Char) : Term
        data class StringLiteral(val value: String) : Term
        data class Var(val v: String, val hole: Boolean = false) : Term
        data class Data(val name: String, val type: Localised<Term>): Term
        data class Forall(
            val v: String?,
            val bound: Localised<Term>,
            val body: Localised<Term>,
            val implicit: Boolean,
        ) : Term

        data class Apply(val lhd: Localised<Term>, val rhd: Localised<Term>, val implicit: Boolean) : Term
        data class Lambda(val v: String, val body: Localised<Term>, val implicit: Boolean) : Term
        data class Exists(val v: String?, val bound: Localised<Term>, val body: Localised<Term>) : Term
        data class Couple(val lhd: Localised<Term>, val rhd: Localised<Term>) : Term
        data class Disjunction(val lhd: Localised<Term>, val rhd: Localised<Term>) : Term
        data class Case(val term: Localised<Term>, val lhd: Localised<Term>, val rhd: Localised<Term>) : Term
        data class Rec(val v: String, val body: Localised<Term>) : Term
        enum class Operation { inl, inr, fst, snd, fold, unfold }
        data class SpecialApp(val operation: Operation, val term: Localised<Term>) : Term
        data class LetBinding(val name: String, val value: Localised<Term>, val body: Localised<Term>): Term

        private fun pretty(): String = when (this) {
            is Type -> "type$level"
            is IntLiteral -> "$value"
            is CharLiteral ->
                if (value == '\'') {
                    "'\\''"
                } else {
                    "'$value'"
                }
            is StringLiteral -> "\"$value\""
            is Var -> if (hole) {
                "?$v"
            } else {
                v
            }
            is Data ->
                "data $name : ${type.value.pretty(true)}"
            is Forall -> {
                if (v == null) "${bound.prettyTerm()} -> ${body.prettyTerm(true)}"
                else if (implicit) "{$v:${bound.prettyTerm()}} -> ${body.prettyTerm()}"
                else "($v:${bound.prettyTerm()}) -> ${body.prettyTerm()}"
            }
            is Apply -> {
                if (implicit) "${lhd.prettyTerm()} {${rhd.prettyTerm()}}"
                else "${lhd.prettyTerm()} ${rhd.prettyTerm(true)}"
            }
            is Lambda ->
                if (implicit) "{$v}.${body.prettyTerm(true)}"
                else "($v).${body.prettyTerm(true)}"
            is Exists -> if (v == null) "${bound.prettyTerm()} * ${body.prettyTerm(true)}"
            else "($v:${bound.prettyTerm()}) * ${body.prettyTerm()}"
            is Couple -> "${lhd.prettyTerm(true)} , ${rhd.prettyTerm(true)}"
            is Disjunction ->
                "${lhd.prettyTerm(true)} | ${rhd.prettyTerm(true)}"
            is Case ->
                "case ${term.prettyTerm(true)} ${lhd.prettyTerm(true)} ${rhd.prettyTerm(true)}"
            is Rec ->
                "rec($v).${body.prettyTerm(true)}"
            is SpecialApp ->
                "$operation ${term.prettyTerm(true)}"
            is LetBinding ->
                "let $name = ${value.prettyTerm()} in ${body.prettyTerm(true)}"
        }

        private fun isAtom(): Boolean = when (this) {
            is Type -> true
            is IntLiteral -> true
            is CharLiteral -> true
            is StringLiteral -> true
            is Var -> true
            is Data -> true
            is Forall -> false
            is Apply -> false
            is Lambda -> true
            is Exists -> false
            is Couple -> false
            is Disjunction -> false
            is Case -> true
            is Rec -> true
            is SpecialApp -> true
            is LetBinding -> false
        }

        fun pretty(atom: Boolean = false): String = if (atom && !isAtom()) "(${pretty()})" else pretty()
    }

    data class Localised<V>(val value: V, val region: Region.T)

    fun Localised<Term>.prettyTerm(atom: Boolean = false) = value.pretty(atom)

    fun Localised<Binding>.prettyBinding(atom: Boolean = false) = value.pretty(atom)
}