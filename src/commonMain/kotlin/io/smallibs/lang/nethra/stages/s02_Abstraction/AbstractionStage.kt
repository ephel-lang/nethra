package io.smallibs.lang.nethra.stages.s02_Abstraction

import io.smallibs.lang.nethra.ast.Ast
import io.smallibs.lang.nethra.ast.Builder
import io.smallibs.lang.nethra.cst.Cst
import io.smallibs.lang.nethra.stages.common.Stage
import io.smallibs.lang.nethra.stages.s03_Checker.internal.Bindings

class AbstractionStage<C>(
    val builder: Builder<C> = Builder(),
) : Stage<List<Cst.Localised<Cst.Binding>>, Bindings<C>>, Builder<C> by builder {
    private fun Cst.Term.compile(): Ast.Term<C> = when (this) {
        is Cst.Term.Type -> type(level)
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
        is Cst.Term.Couple -> pair(lhd.value.compile(), rhd.value.compile())
        is Cst.Term.Disjunction -> or(lhd.value.compile(), rhd.value.compile())
        is Cst.Term.Case -> case(term.value.compile(), lhd.value.compile(), rhd.value.compile())
        is Cst.Term.Rec -> rec(v, body.value.compile())
        is Cst.Term.SpecialApp ->
            when (operation) {
                Cst.Term.Operation.inl -> inl(term.value.compile())
                Cst.Term.Operation.inr -> inr(term.value.compile())
                Cst.Term.Operation.fst -> fst(term.value.compile())
                Cst.Term.Operation.snd -> snd(term.value.compile())
                Cst.Term.Operation.fold -> fold(term.value.compile())
                Cst.Term.Operation.unfold -> unfold(term.value.compile())
            }
    }

    private fun compile(i: Cst.Localised<Cst.Binding>): Ast.Binding<C> = when (val binding = i.value) {
        is Cst.Binding.Signature -> Ast.Binding.Signature(binding.name, binding.value.value.compile())
        is Cst.Binding.Definition -> Ast.Binding.Definition(binding.name, binding.value.value.compile())
    }

    override infix fun compile(i: List<Cst.Localised<Cst.Binding>>): Bindings<C> = i.map(::compile).let { bindings ->
        Bindings(bindings.filterIsInstance<Ast.Binding.Signature<C>>().associate { it.name to it.value },
            bindings.filterIsInstance<Ast.Binding.Definition<C>>().associate { it.name to it.value })
    }

    override infix fun decompile(o: Bindings<C>): List<Cst.Localised<Cst.Binding>> {
        TODO("Not yet implemented")
    }

}
