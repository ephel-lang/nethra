package io.smallibs.lang.nethra.stages.s02_Abstraction

import io.smallibs.lang.nethra.ast.Ast
import io.smallibs.lang.nethra.ast.Ast.Term.Companion.ANON
import io.smallibs.lang.nethra.ast.Builder
import io.smallibs.lang.nethra.ast.Substitution
import io.smallibs.lang.nethra.cst.Cst
import io.smallibs.lang.nethra.stages.common.Stage
import io.smallibs.lang.nethra.stages.s03_Checker.internal.Bindings
import io.smallibs.parsec.parser.Region

class AbstractionStage(
    val builder: Builder<Region.T> = Builder(),
    val substitution: Substitution<Region.T> = Substitution(),
) : Stage<List<Cst.Localised<Cst.Binding>>, Bindings<Region.T>>, Substitution<Region.T> by substitution,
    Builder<Region.T> by builder {
    private fun Cst.Localised<Cst.Term>.compile(): Ast.Term<Region.T> =
        this.value.compile().set(this.region)

    private fun Cst.Term.compile(): Ast.Term<Region.T> = when (this) {
        is Cst.Term.Type -> type(level)
        is Cst.Term.Var -> id(v)
        is Cst.Term.Data -> TODO()
        is Cst.Term.CharLiteral -> char(value)
        is Cst.Term.IntLiteral -> int(value)
        is Cst.Term.StringLiteral -> string(value)
        is Cst.Term.Forall -> pi(this.v ?: ANON, bound.compile(), body.compile(), implicit)
        is Cst.Term.Apply -> apply(lhd.compile(), rhd.compile(), implicit)
        is Cst.Term.Lambda -> lambda(v, body.compile(), implicit)
        is Cst.Term.Exists -> sigma(this.v ?: ANON, bound.compile(), body.compile())
        is Cst.Term.Couple -> pair(lhd.compile(), rhd.compile())
        is Cst.Term.Disjunction -> or(lhd.compile(), rhd.compile())
        is Cst.Term.Case -> case(term.compile(), lhd.compile(), rhd.compile())
        is Cst.Term.Rec -> rec(v, body.compile())
        is Cst.Term.SpecialApp ->
            when (operation) {
                Cst.Term.Operation.inl -> inl(term.compile())
                Cst.Term.Operation.inr -> inr(term.compile())
                Cst.Term.Operation.fst -> fst(term.compile())
                Cst.Term.Operation.snd -> snd(term.compile())
                Cst.Term.Operation.fold -> fold(term.compile())
                Cst.Term.Operation.unfold -> unfold(term.compile())
            }
        is Cst.Term.LetBinding ->
            body.compile().substitute(name to value.compile())
    }

    private fun compile(i: Cst.Localised<Cst.Binding>): Ast.Binding<Region.T> = when (val binding = i.value) {
        is Cst.Binding.Signature -> Ast.Binding.Signature(binding.name, binding.value.compile())
        is Cst.Binding.Definition -> Ast.Binding.Definition(binding.name, binding.value.compile())
    }

    override infix fun compile(i: List<Cst.Localised<Cst.Binding>>): Bindings<Region.T> =
        i.map { compile(it).set(it.region) }.let { bindings ->
            Bindings(
                bindings.filterIsInstance<Ast.Binding.Signature<Region.T>>().associate { it.name to it.value },
                bindings.filterIsInstance<Ast.Binding.Definition<Region.T>>().associate { it.name to it.value }
            )
        }

}
