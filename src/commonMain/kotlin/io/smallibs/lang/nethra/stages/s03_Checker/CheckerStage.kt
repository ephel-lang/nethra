package io.smallibs.lang.nethra.stages.s03_Checker

import io.smallibs.lang.nethra.ast.Ast
import io.smallibs.lang.nethra.ast.Builder
import io.smallibs.lang.nethra.ast.Congruence
import io.smallibs.lang.nethra.ast.Printer
import io.smallibs.lang.nethra.stages.common.Stage
import io.smallibs.lang.nethra.stages.s03_Checker.internal.Bindings
import io.smallibs.lang.nethra.stages.s03_Checker.internal.Checker
import io.smallibs.lang.nethra.stages.s03_Checker.internal.Inference

class CheckerStage<C>(
    private val checker: Checker<C> = Checker(),
    private val printer: Printer<C> = Printer(),
    private val congruence: Congruence<C> = Congruence(),
) : Stage<Bindings<C>, Bindings<C>>, Checker<C> by checker, Printer<C> by printer, Congruence<C> by congruence {

    override infix fun compile(bindings: Bindings<C>): Bindings<C> = with(Inference(checker)) {
        bindings.gamma.map { (_, type) ->
            bindings.infer(type)
        }.let {
            bindings.delta.map { (name, definition) ->
                val type = bindings.getSignature(name) ?: throw Exception("No specification for $name")
                if (!bindings.check(definition, type)) {
                    throw Exception("${definition.prettyPrint()} not a ${type.prettyPrint()}")
                }
            }
        }.let {
            bindings
        }
    }

    override infix fun decompile(o: Bindings<C>): Bindings<C> = o
}
