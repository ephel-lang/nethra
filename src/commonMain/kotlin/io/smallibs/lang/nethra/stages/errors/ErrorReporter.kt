package io.smallibs.lang.nethra.stages.errors

import io.smallibs.lang.nethra.stages.errors.impl.ErrorReporterImpl

interface ErrorReporter {

    fun report(error: CompilationException)
    fun errors() : Int

    companion object {
        operator fun invoke(source: String): ErrorReporter = ErrorReporterImpl(source)
    }
}