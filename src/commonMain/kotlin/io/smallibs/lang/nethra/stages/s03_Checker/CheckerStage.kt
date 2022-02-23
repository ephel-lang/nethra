package io.smallibs.lang.nethra.stages.s03_Checker

import io.smallibs.lang.nethra.ast.Ast
import io.smallibs.lang.nethra.ast.Printer
import io.smallibs.lang.nethra.stages.common.Stage
import io.smallibs.lang.nethra.stages.s03_Checker.internal.Checker
import io.smallibs.lang.nethra.stages.s03_Checker.internal.Context

class CheckerStage<C>(
    private val checker: Checker<C> = Checker(),
    private val printer: Printer<C> = Printer()
) : Stage<List<Ast.Binding<C>>, List<Ast.Binding<C>>>, Checker<C> by checker, Printer<C> by printer {

    override infix fun compile(i: List<Ast.Binding<C>>): List<Ast.Binding<C>> =
        i.filterIsInstance<Ast.Binding.Signature<C>>().associate { it.name to it.value }.let {
            Context(it)
        }.let { gamma ->
            i.filterIsInstance<Ast.Binding.Definition<C>>().map {
                val type = gamma.getSignature(it.name) ?: throw Exception("No specification for ${it.name}")
                if (!gamma.check(it.value, type)) {
                    throw Exception("${it.value.prettyPrint()} not a ${type.prettyPrint()}")
                }
            }
        }.let {
            i
        }

    override infix fun decompile(o: List<Ast.Binding<C>>): List<Ast.Binding<C>> = o
}
