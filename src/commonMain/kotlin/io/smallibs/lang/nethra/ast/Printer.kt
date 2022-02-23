package io.smallibs.lang.nethra.ast

import io.smallibs.lang.nethra.ast.impl.PrinterImpl

interface Printer<C> {
    fun Ast.Term<C>.prettyPrint(): String

    companion object {
        operator fun <C> invoke(): Printer<C> = PrinterImpl()
    }
}
