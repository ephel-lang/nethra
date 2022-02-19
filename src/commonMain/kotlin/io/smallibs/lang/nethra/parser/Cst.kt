package io.smallibs.lang.nethra.parser

import io.smallibs.parsec.parser.Region

object Cst {

    sealed interface Type {
        object Kind : Type
        object IntLiteral : Type
        object CharLiteral : Type
        data class Fun(val v: String?, val bound: Localised, val body: Localised, val implicit: Boolean) : Type
        data class Apply(val ldh: Localised, val rhd: Localised, val implicit: Boolean) : Type
        data class Tuple(val ldh: Localised, val rhd: Localised) : Type
        data class Var(val v: String) : Type

        data class Localised(val type: Type, val region: Region.T)

        fun pretty(): String =
            when (this) {
                Kind -> "*"
                CharLiteral -> "Char"
                IntLiteral -> "Int"
                is Fun -> {
                    if (this.v == null)
                        if (this.bound.type.isAtom())
                            "${this.bound.pretty()} -> ${this.body.pretty()}"
                        else
                            "(${this.bound.pretty()}) -> ${this.body.pretty()}"
                    else if (implicit)
                        "{$v:${this.bound.pretty()}} -> ${this.body.pretty()}"
                    else
                        "($v:${this.bound.pretty()}) -> ${this.body.pretty()}"
                }
                is Tuple -> "${this.ldh.pretty()},${this.ldh.pretty()}"
                is Apply -> {
                    if (implicit)
                        "${this.ldh.pretty()} {${this.rhd.pretty()}}"
                    else
                        "${this.ldh.pretty()} (${this.rhd.pretty()})"
                }
                is Var -> this.v
            }

        fun isAtom(): Boolean =
            when (this) {
                is Apply -> false
                CharLiteral -> true
                is Fun -> false
                IntLiteral -> true
                Kind -> true
                is Tuple -> false
                is Var -> true
            }
    }

    fun Type.Localised.pretty() = this.type.pretty()

}