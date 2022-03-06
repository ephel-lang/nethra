package io.smallibs.lang.nethra.stages.s03_Checker

import io.smallibs.lang.nethra.ast.Ast
import io.smallibs.lang.nethra.ast.Builder
import io.smallibs.lang.nethra.ast.Congruence
import io.smallibs.lang.nethra.stages.common.Stage
import io.smallibs.lang.nethra.stages.errors.CompilationException
import io.smallibs.lang.nethra.stages.errors.ErrorReporter
import io.smallibs.lang.nethra.stages.s03_Checker.internal.Bindings
import io.smallibs.lang.nethra.stages.s03_Checker.internal.Checker
import io.smallibs.lang.nethra.stages.s03_Checker.internal.Inference

class CheckerStage<C>(
    private val reporter: ErrorReporter,
    private val builder: Builder<C> = Builder(),
    private val checker: Checker<C> = Checker(),
    private val congruence: Congruence<C> = Congruence(),
) : Stage<Bindings<C>, Bindings<C>>, Builder<C> by builder, Checker<C> by checker, Congruence<C> by congruence, ErrorReporter by reporter {

    override infix fun compile(bindings: Bindings<C>): Bindings<C> = with(Inference(checker)) {
        bindings.gamma.map { (_, type) ->
            try {
                bindings.check(bindings.infer(type), type(2))
            } catch (e: CompilationException) {
                report(e)
            }
        }.let {
            bindings.delta.map { (name, definition) ->
                val type = bindings.getSignature(name) ?: throw CompilationException.Unbound(Ast.Term.Id<C>(name)
                    .set(definition.context))
                try {
                    bindings.check(definition, type)
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
