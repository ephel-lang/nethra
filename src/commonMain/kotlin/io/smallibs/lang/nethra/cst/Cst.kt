package io.smallibs.lang.nethra.cst

import io.smallibs.parsec.parser.Region

object Cst {

    sealed interface Binding {
        val name: String
        val value: Term.Localised

        data class Signature(override val name: String, override val value: Term.Localised) : Binding
        data class Definition(override val name: String, override val value: Term.Localised) : Binding

        fun pretty(atom: Boolean = false): String =
            when (this) {
                is Signature -> "sig $name : ${value.pretty()}"
                is Definition -> "def $name = ${value.pretty()}"
            }
    }

    sealed interface Term {
        object Type : Term
        object IntTypeLiteral : Term
        object CharTypeLiteral : Term
        object StringTypeLiteral : Term
        data class IntLiteral(val value: Int) : Term
        data class CharLiteral(val value: Char) : Term
        data class StringLiteral(val value: String) : Term
        data class Var(val v: String, val hole: Boolean = false) : Term
        data class Forall(val v: String?, val bound: Localised, val body: Localised, val implicit: Boolean) : Term
        data class Apply(val lhd: Localised, val rhd: Localised, val implicit: Boolean) : Term
        data class Exists(val v: String?, val bound: Localised, val body: Localised) : Term
        data class Lambda(val v: String, val body: Localised, val implicit: Boolean) : Term
        data class Disjunction(val lhd: Localised, val rhd: Localised) : Term
        data class Case(val term: Localised, val lhd: Localised, val rhd: Localised) : Term

        data class Localised(val term: Term, val region: Region.T)

        private fun pretty(): String = when (this) {
            Type -> "Type"
            IntTypeLiteral -> "Int"
            CharTypeLiteral -> "Char"
            StringTypeLiteral -> "String"
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
            is Forall -> {
                if (v == null) "${bound.pretty()} -> ${body.pretty(true)}"
                else if (implicit) "{$v:${bound.pretty()}} -> ${body.pretty()}"
                else "($v:${bound.pretty()}) -> ${body.pretty()}"
            }
            is Apply -> {
                if (implicit) "${lhd.pretty()} {${rhd.pretty()}}"
                else "${lhd.pretty()} ${rhd.pretty(true)}"
            }
            is Exists -> if (v == null) "${bound.pretty()} * ${body.pretty(true)}"
            else "($v:${bound.pretty()}) * ${body.pretty()}"
            is Lambda ->
                if (implicit) "{$v}.${body.pretty(true)}"
                else "($v).${body.pretty(true)}"
            is Disjunction ->
                "${lhd.pretty()} | ${rhd.pretty(true)}"
            is Case ->
                "case ${term.pretty(true)} ${lhd.pretty(true)} ${rhd.pretty(true)}"
        }

        private fun isAtom(): Boolean = when (this) {
            Type -> true
            IntTypeLiteral -> true
            CharTypeLiteral -> true
            StringTypeLiteral -> true
            is IntLiteral -> true
            is CharLiteral -> true
            is StringLiteral -> true
            is Var -> true
            is Forall -> false
            is Apply -> false
            is Exists -> false
            is Lambda -> true
            is Disjunction -> false
            is Case -> true
        }

        fun pretty(atom: Boolean = false): String = if (atom && !isAtom()) "(${pretty()})" else pretty()
    }

    fun Term.Localised.pretty(atom: Boolean = false) = term.pretty(atom)

}