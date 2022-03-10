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
                is Signature -> "sig $name : ${value.prettyPrint()}"
                is Definition -> "def $name = ${value.prettyPrint()}"
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
                if (v == null) "${bound.prettyPrint()} -> ${body.prettyPrint(true)}"
                else if (implicit) "{$v:${bound.prettyPrint()}} -> ${body.prettyPrint()}"
                else "($v:${bound.prettyPrint()}) -> ${body.prettyPrint()}"
            }
            is Apply -> {
                if (implicit) "${lhd.prettyPrint()} {${rhd.prettyPrint()}}"
                else "${lhd.prettyPrint()} ${rhd.prettyPrint(true)}"
            }
            is Lambda ->
                if (implicit) "{$v}.${body.prettyPrint(true)}"
                else "($v).${body.prettyPrint(true)}"
            is Exists -> if (v == null) "${bound.prettyPrint()} * ${body.prettyPrint(true)}"
            else "($v:${bound.prettyPrint()}) * ${body.prettyPrint()}"
            is Couple -> "${lhd.prettyPrint(true)} , ${rhd.prettyPrint(true)}"
            is Disjunction ->
                "${lhd.prettyPrint(true)} | ${rhd.prettyPrint(true)}"
            is Case ->
                "case ${term.prettyPrint(true)} ${lhd.prettyPrint(true)} ${rhd.prettyPrint(true)}"
            is Rec ->
                "rec($v).${body.prettyPrint(true)}"
            is SpecialApp ->
                "$operation ${term.prettyPrint(true)}"
            is LetBinding ->
                "let $name = ${value.prettyPrint()} in ${body.prettyPrint(true)}"
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

    fun Localised<Term>.prettyPrint(atom: Boolean = false) = value.pretty(atom)

    fun Localised<Binding>.prettyBinding(atom: Boolean = false) = value.pretty(atom)
}