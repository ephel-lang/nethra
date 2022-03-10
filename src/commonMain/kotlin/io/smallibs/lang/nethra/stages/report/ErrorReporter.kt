package io.smallibs.lang.nethra.stages.report

import io.smallibs.lang.nethra.stages.report.impl.ErrorReporterImpl

interface ErrorReporter {

    fun report(error: CompilationException)
    fun errors() : Int

    companion object {
        operator fun invoke(source: String): ErrorReporter = ErrorReporterImpl(source)
    }
}