package io.smallibs.lang.nethra.stages.s02_Abstraction

import io.smallibs.lang.nethra.ast.Ast
import io.smallibs.lang.nethra.cst.Cst
import io.smallibs.lang.nethra.stages.common.Stage
import io.smallibs.parsec.parser.Region

class TermConcretizationStage : Stage<Ast.Term<Region.T>, Cst.Localised<Cst.Term>> {

    override fun act(i: Ast.Term<Region.T>): Cst.Localised<Cst.Term> =
        when(i) {
            is Ast.Term.Apply -> Cst.Localised(Cst.Term.Apply(act(i.abstraction), act(i.argument), i.implicit), i.context ?: Region.none)
            is Ast.Term.Case -> Cst.Localised(Cst.Term.Case(act(i.term), act(i.left), act(i.right)), i.context ?: Region.none)
            is Ast.Term.Couple -> Cst.Localised(Cst.Term.Couple(act(i.lhd), act(i.rhd)), i.context ?: Region.none)
            is Ast.Term.Disjunction -> Cst.Localised(Cst.Term.Disjunction(act(i.lhd), act(i.rhd)), i.context ?: Region.none)
            is Ast.Term.Fold -> Cst.Localised(Cst.Term.SpecialApp(Cst.Term.Operation.fold, act(i.term)), i.context ?: Region.none)
            is Ast.Term.Fst -> Cst.Localised(Cst.Term.SpecialApp(Cst.Term.Operation.fst, act(i.term)), i.context ?: Region.none)
            is Ast.Term.Hole -> Cst.Localised(Cst.Term.Var(i.external ?: i.value), i.context ?: Region.none) // TODO
            is Ast.Term.Id -> Cst.Localised(Cst.Term.Var(i.value), i.context ?: Region.none)
            is Ast.Term.Inl -> Cst.Localised(Cst.Term.SpecialApp(Cst.Term.Operation.inl, act(i.term)), i.context ?: Region.none)
            is Ast.Term.Inr -> Cst.Localised(Cst.Term.SpecialApp(Cst.Term.Operation.inr, act(i.term)), i.context ?: Region.none)
            is Ast.Term.Lambda -> Cst.Localised(Cst.Term.Lambda(i.n.value, act(i.body), i.implicit), i.context ?: Region.none)
            is Ast.Term.Lit ->
                when(i.literal) {
                    is Ast.Term.Literal.CharLit -> Cst.Localised(Cst.Term.CharLiteral(i.literal.value), i.context ?: Region.none)
                    is Ast.Term.Literal.IntLit -> Cst.Localised(Cst.Term.IntLiteral(i.literal.value), i.context ?: Region.none)
                    is Ast.Term.Literal.StringLit -> Cst.Localised(Cst.Term.StringLiteral(i.literal.value), i.context ?: Region.none)
                }
            is Ast.Term.Pi -> Cst.Localised(Cst.Term.Forall(name(i.n.value), act(i.bound), act(i.body), i.implicit), i.context ?: Region.none)
            is Ast.Term.Rec -> Cst.Localised(Cst.Term.Rec(i.self.value, act(i.body)), i.context ?: Region.none)
            is Ast.Term.Sigma -> Cst.Localised(Cst.Term.Exists(name(i.n.value), act(i.bound), act(i.body)), i.context ?: Region.none)
            is Ast.Term.Snd -> Cst.Localised(Cst.Term.SpecialApp(Cst.Term.Operation.snd, act(i.term)), i.context ?: Region.none)
            is Ast.Term.Type -> Cst.Localised(Cst.Term.Type(i.level), i.context ?: Region.none)
            is Ast.Term.Unfold -> Cst.Localised(Cst.Term.SpecialApp(Cst.Term.Operation.unfold, act(i.term)), i.context ?: Region.none)
        }

    private fun name(v: String) : String? = if (v == "_") null else v

}
