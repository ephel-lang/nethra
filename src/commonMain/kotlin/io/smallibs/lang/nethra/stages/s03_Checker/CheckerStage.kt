package io.smallibs.lang.nethra.stages.s03_Checker

import io.smallibs.lang.nethra.ast.Builder
import io.smallibs.lang.nethra.ast.Congruence
import io.smallibs.lang.nethra.stages.common.Stage
import io.smallibs.lang.nethra.stages.report.CompilationException
import io.smallibs.lang.nethra.stages.report.ErrorReporter
import io.smallibs.lang.nethra.stages.report.ProofPrinter
import io.smallibs.lang.nethra.stages.s03_Checker.internal.Bindings
import io.smallibs.lang.nethra.stages.s03_Checker.internal.Checker
import io.smallibs.lang.nethra.stages.s03_Checker.internal.Inference

class CheckerStage<C>(
    private val reporter: ErrorReporter,
    private val builder: Builder<C> = Builder(),
    private val checker: Checker<C> = Checker(),
    private val congruence: Congruence<C> = Congruence(),
) : Stage<Bindings<C>, Bindings<C>>, Builder<C> by builder, Checker<C> by checker, Congruence<C> by congruence,
    ErrorReporter by reporter {

    override infix fun compile(bindings: Bindings<C>): Bindings<C> = with(Inference(checker)) {
        bindings.gamma.map { (_, type) ->
            try {
                val check = bindings.check(type, type(2))
                if (!check.success()) {
                    ProofPrinter<C>().printError(check)
                    println()
                    check.error()
                }
            } catch (e: CompilationException) {
                report(e)
            }
        }.let {
            bindings.delta.map { (name, definition) ->
                try {
                    val type = bindings.getSignature(name) ?: throw CompilationException.Unbound(id(name).set(definition.context))
                    val check = bindings.check(definition, type)
                    if (!check.success()) {
                        ProofPrinter<C>().printError(check)
                        println()
                        check.error()
                    }
                } catch (e: CompilationException) {
                    report(e)
                }
            }
        }.let {
            if (errors() > 0) {
                throw Exception()
            }
            bindings
        }
    }

}
