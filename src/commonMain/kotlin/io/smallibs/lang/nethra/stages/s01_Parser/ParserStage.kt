package io.smallibs.lang.nethra.stages.s01_Parser

import io.smallibs.lang.nethra.cst.Cst
import io.smallibs.lang.nethra.cst.Cst.prettyBinding
import io.smallibs.lang.nethra.stages.common.Stage
import io.smallibs.lang.nethra.stages.errors.CompilationException
import io.smallibs.lang.nethra.stages.errors.ErrorReporter
import io.smallibs.lang.nethra.stages.s01_Parser.internal.BindingParser
import io.smallibs.parsec.io.Reader.Companion.string
import io.smallibs.parsec.parser.Flow.eos
import io.smallibs.parsec.parser.Flow.optrep
import io.smallibs.parsec.parser.Flow.thenLeft

class ParserStage(private val reporter: ErrorReporter) : Stage<String, List<Cst.Localised<Cst.Binding>>> {
    override infix fun compile(i: String): List<Cst.Localised<Cst.Binding>> =
        (BindingParser().optrep thenLeft eos())(string(i)).fold({ it.value }, {
            val error = CompilationException.SyntaxError(it.location)
            reporter.report(error)
            throw error
        })

    infix fun decompile(o: List<Cst.Localised<Cst.Binding>>): String =
        o.joinToString(separator = "\n", transform = { it.prettyBinding() })
}