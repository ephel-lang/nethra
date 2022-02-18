package io.smallibs.lang.nethra.parser

import io.smallibs.parsec.parser.Region

object Cst {


    sealed interface Type {
        object Kind : Type
        object IntLiteral : Type
        object CharLiteral : Type
        data class Fun(val v: String?, val bound: Localised, val body: Localised, val implicit : Boolean) : Type
        data class Var(val v: String) : Type

        data class Localised(val type: Type, val region: Region.T)
    }

}