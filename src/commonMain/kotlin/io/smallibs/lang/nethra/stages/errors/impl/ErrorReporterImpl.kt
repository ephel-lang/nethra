package io.smallibs.lang.nethra.stages.errors.impl

import io.smallibs.lang.nethra.ast.Ast
import io.smallibs.lang.nethra.ast.Printer
import io.smallibs.lang.nethra.stages.errors.CompilationException
import io.smallibs.lang.nethra.stages.errors.ErrorReporter
import io.smallibs.parsec.parser.Region
import io.smallibs.parsec.utils.Location
import kotlin.math.max

data class ErrorReporterImpl(private val source: String) : ErrorReporter {

    private var errors = 0

    @Suppress("UNCHECKED_CAST")
    override fun report(error: CompilationException) {
        errors += 1
        with(Printer<Region.T>()) {
            when (error) {
                is CompilationException.SyntaxError -> {
                    println("| error $errors: line ${error.location.line}, character ${error.location.column}")
                    println("| reason> " + fragment(error.location))
                }
                is CompilationException.CannotCheck -> {
                    display(error.term as Ast.Term<Region.T>)

                    val type = error.type as Ast.Term<Region.T>
                    println("| reason: a ${type.prettyPrint()} is expected")
                }
                is CompilationException.CannotInfer -> {
                    display(error.term as Ast.Term<Region.T>)
                    println("| reason: cannot infer type")
                }
                is CompilationException.Unbound -> {
                    display(error.term as Ast.Term<Region.T>)
                    println("| reason: unbound identifier ${error.term.prettyPrint()}")
                }
                is CompilationException.CannotCompare -> {
                    display(error.term as Ast.Term<Region.T>)

                    val computed = error.computed as Ast.Term<Region.T>
                    val expected = error.expected as Ast.Term<Region.T>
                    println("| reason: has type ${computed.prettyPrint()} but waiting for ${expected.prettyPrint()}")
                }
            }
            println()
        }
    }

    override fun errors(): Int = errors

    private fun display(term: Ast.Term<Region.T>) {
        val region = term.context ?: Region.T(Location(), Location())
        println("| error $errors: line ${region.begin.line}, characters ${region.begin.column}-${region.end.column}")
        fragment(region).let {
            println("|\t${it.first}${it.second.first}${it.second.second}".trimEnd())
            println("|\t" + " ".repeat(it.first.length) + "^".repeat(it.second.first.length) + " ".repeat(it.second.second.trimEnd().length))
        }
    }

    private fun fragment(region: Region.T): Pair<String, Pair<String, String>> {
        val line = source.split("\n")[region.begin.line - 1]
        val start = region.begin.column
        val end = if (region.end.line !=  region.begin.line) line.length else region.end.column
        return line.subSequence(0, start).toString() to
                (line.subSequence(start, end).toString() to
                        line.subSequence(end, line.length).toString())
    }

    private fun fragment(location: Location): String {
        val start = location.position - location.column
        val end = location.position
        return source.subSequence(start, end).toString()
    }
}