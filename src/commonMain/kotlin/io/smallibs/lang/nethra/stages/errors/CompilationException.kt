package io.smallibs.lang.nethra.stages.errors

import io.smallibs.lang.nethra.ast.Ast
import io.smallibs.parsec.utils.Location

sealed class CompilationException : Exception() {
    data class SyntaxError(val location: Location) : CompilationException()

    data class Unbound(val term: Ast.Term<*>) : CompilationException()
    data class CannotCheck(val term: Ast.Term<*>, val type: Ast.Term<*>) : CompilationException()
    data class CannotInfer(val term: Ast.Term<*>) : CompilationException()
    data class CannotCompare(val term: Ast.Term<*>, val expected: Ast.Term<*>, val computed: Ast.Term<*>) : CompilationException()
}
