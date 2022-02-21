package io.smallibs.lang.nethra.parser

import io.smallibs.parsec.parser.Region

object Cst {

    sealed interface Type {
        object Kind : Type
        object IntLiteral : Type
        object CharLiteral : Type
        data class Var(val v: String, val hole: Boolean = false) : Type
        data class Forall(val v: String?, val bound: Localised, val body: Localised, val implicit: Boolean) : Type
        data class Apply(val ldh: Localised, val rhd: Localised, val implicit: Boolean) : Type
        data class Exists(val v: String?, val bound: Localised, val body: Localised) : Type

        data class Localised(val type: Type, val region: Region.T)

        private fun pretty(): String =
            when (this) {
                Kind -> "Type"
                IntLiteral -> "Int"
                CharLiteral -> "Char"
                is Var ->
                    if (hole) {
                        "?$v"
                    } else {
                        v
                    }
                is Forall -> {
                    if (v == null)
                        "${bound.pretty(true)} -> ${body.pretty(true)}"
                    else if (implicit)
                        "{$v:${bound.pretty()}} -> ${body.pretty()}"
                    else
                        "($v:${bound.pretty()}) -> ${body.pretty()}"
                }
                is Apply -> {
                    if (implicit)
                        "${ldh.pretty()} {${rhd.pretty()}}"
                    else
                        "${ldh.pretty()} ${rhd.pretty(true)}"
                }
                is Exists ->
                    if (v == null)
                        "${bound.pretty(true)} * ${body.pretty(true)}"
                    else
                        "($v:${bound.pretty()}) * ${body.pretty()}"

            }

        private fun isAtom(): Boolean =
            when (this) {
                Kind -> true
                IntLiteral -> true
                CharLiteral -> true
                is Var -> true
                is Forall -> false
                is Apply -> false
                is Exists -> false
            }

        fun pretty(atom: Boolean = false): String =
            if (atom && !isAtom()) "(${pretty()})" else pretty()
    }

    fun Type.Localised.pretty(atom: Boolean = false) = type.pretty(atom)

}