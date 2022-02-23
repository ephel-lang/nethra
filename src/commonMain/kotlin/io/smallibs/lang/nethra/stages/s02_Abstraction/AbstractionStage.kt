package io.smallibs.lang.nethra.stages.s02_Abstraction

import io.smallibs.lang.nethra.ast.Ast
import io.smallibs.lang.nethra.ast.Builder
import io.smallibs.lang.nethra.cst.Cst
import io.smallibs.lang.nethra.stages.common.Stage

class AbstractionStage(
    val builder: Builder<Nothing> = Builder(),
) : Stage<List<Cst.Localised<Cst.Binding>>, List<Ast.Binding<Nothing>>>, Builder<Nothing> by builder {
    private fun Cst.Term.compile(): Ast.Term<Nothing> =
        when (this) {
            Cst.Term.Type -> type()
            Cst.Term.IntTypeLiteral -> data("int", type())
            Cst.Term.CharTypeLiteral -> data("char", type())
            Cst.Term.StringTypeLiteral -> data("string", type())
            is Cst.Term.Var -> id(v)
            is Cst.Term.CharLiteral -> char(value)
            is Cst.Term.IntLiteral -> int(value)
            is Cst.Term.StringLiteral -> string(value)
            is Cst.Term.Forall -> pi(this.v ?: "_", bound.value.compile(), body.value.compile(), implicit)
            is Cst.Term.Apply -> apply(lhd.value.compile(), rhd.value.compile(), implicit)
            is Cst.Term.Lambda -> lambda(v, body.value.compile(), implicit)
            is Cst.Term.Exists -> sigma(this.v ?: "_", bound.value.compile(), body.value.compile())
            is Cst.Term.Disjunction -> or(lhd.value.compile(), rhd.value.compile())
            is Cst.Term.Case -> case(term.value.compile(), lhd.value.compile(), rhd.value.compile())
        }

    private fun compile(i: Cst.Localised<Cst.Binding>): Ast.Binding<Nothing> =
        when (val binding = i.value) {
            is Cst.Binding.Signature -> Ast.Binding.Signature(binding.name, binding.value.value.compile())
            is Cst.Binding.Definition -> Ast.Binding.Definition(binding.name, binding.value.value.compile())
        }

    override infix fun compile(i: List<Cst.Localised<Cst.Binding>>): List<Ast.Binding<Nothing>> =
        i.map(::compile)

    override infix fun decompile(o: List<Ast.Binding<Nothing>>): List<Cst.Localised<Cst.Binding>> {
        TODO("Not yet implemented")
    }

}
