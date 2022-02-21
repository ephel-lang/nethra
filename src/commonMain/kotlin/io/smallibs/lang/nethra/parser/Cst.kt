package io.smallibs.lang.nethra.parser

import io.smallibs.parsec.parser.Region

object Cst {

    sealed interface Term {
        object Kind : Term
        object IntLiteral : Term
        object CharLiteral : Term
        data class Var(val v: String, val hole: Boolean = false) : Term
        data class Forall(val v: String?, val bound: Localised, val body: Localised, val implicit: Boolean) : Term
        data class Apply(val ldh: Localised, val rhd: Localised, val implicit: Boolean) : Term
        data class Exists(val v: String?, val bound: Localised, val body: Localised) : Term
        data class Lambda(val v: String, val body: Localised, val implicit: Boolean) : Term

        data class Localised(val term: Term, val region: Region.T)

        private fun pretty(): String = when (this) {
            Kind -> "Type"
            IntLiteral -> "Int"
            CharLiteral -> "Char"
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
                if (implicit) "${ldh.pretty()} {${rhd.pretty()}}"
                else "${ldh.pretty()} ${rhd.pretty(true)}"
            }
            is Exists -> if (v == null) "${bound.pretty()} * ${body.pretty(true)}"
            else "($v:${bound.pretty()}) * ${body.pretty()}"
            is Lambda ->
                if (implicit) "{$v}.${body.pretty(true)}"
                else "($v).${body.pretty(true)}"

        }

        private fun isAtom(): Boolean = when (this) {
            Kind -> true
            IntLiteral -> true
            CharLiteral -> true
            is Var -> true
            is Forall -> false
            is Apply -> false
            is Exists -> false
            is Lambda -> true
        }

        fun pretty(atom: Boolean = false): String = if (atom && !isAtom()) "(${pretty()})" else pretty()
    }

    fun Term.Localised.pretty(atom: Boolean = false) = term.pretty(atom)

}