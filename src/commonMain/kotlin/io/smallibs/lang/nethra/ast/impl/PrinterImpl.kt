package io.smallibs.lang.nethra.ast.impl

import io.smallibs.lang.nethra.ast.Ast
import io.smallibs.lang.nethra.ast.Ast.Term.Companion.ANON
import io.smallibs.lang.nethra.ast.Printer
import io.smallibs.lang.nethra.ast.Visitor

class PrinterImpl<C> : Printer<C>, Visitor<C, Unit, String> {

    override fun Ast.Term<C>.prettyPrint(): String = this.run(Unit)

    /**
     * Visitor implementation
     */

    override fun Ast.Term.Type<C>.run(i: Unit) = "Type_$level"

    override fun Ast.Term.Id<C>.run(i: Unit) = initial ?: value

    override fun Ast.Term.Lit<C>.run(i: Unit) =
        when (this.literal) {
            is Ast.Term.Literal.IntLit -> this.literal.value.toString()
            is Ast.Term.Literal.CharLit -> "'${this.literal.value}'"
            is Ast.Term.Literal.StringLit -> "\"${this.literal.value}\""
        }

    override fun Ast.Term.Pi<C>.run(i: Unit) =
        when (implicit) {
            true -> "Π{${n.prettyPrint()}:${bound.prettyPrint()}}.(${body.prettyPrint()})"
            false ->
                if (n.value == ANON)
                    "${bound.prettyPrint()} -> ${body.prettyPrint()}"
                else
                    "Π(${n.prettyPrint()}:${bound.prettyPrint()}).(${body.prettyPrint()})"
        }

    override fun Ast.Term.Lambda<C>.run(i: Unit) =
        when (implicit) {
            true -> "λ{${n.prettyPrint()}}.(${body.prettyPrint()})"
            false -> "λ(${n.prettyPrint()}).(${body.prettyPrint()})"
        }

    override fun Ast.Term.Apply<C>.run(i: Unit) =
        when (implicit) {
            true -> "${abstraction.prettyPrint()} ({${argument.prettyPrint()}})"
            false -> "${abstraction.prettyPrint()} (${argument.prettyPrint()})"
        }

    override fun Ast.Term.Sigma<C>.run(i: Unit) = "Σ(${n.prettyPrint()}:${bound.prettyPrint()}).(${body.prettyPrint()})"

    override fun Ast.Term.Couple<C>.run(i: Unit) = "(${lhd.prettyPrint()},${rhd.prettyPrint()})"

    override fun Ast.Term.Fst<C>.run(i: Unit): String = "fst ${term.prettyPrint()}"

    override fun Ast.Term.Snd<C>.run(i: Unit): String = "snd ${term.prettyPrint()}"

    override fun Ast.Term.Disjunction<C>.run(i: Unit) = "${lhd.prettyPrint()} + ${rhd.prettyPrint()}"

    override fun Ast.Term.Inl<C>.run(i: Unit) = "inl ${term.prettyPrint()}"

    override fun Ast.Term.Inr<C>.run(i: Unit) = "inr ${term.prettyPrint()}"

    override fun Ast.Term.Case<C>.run(i: Unit) =
        "case ${term.prettyPrint()} ${left.prettyPrint()} ${right.prettyPrint()}"

    override fun Ast.Term.Rec<C>.run(i: Unit) = "μ(${self.prettyPrint()}).(${body.prettyPrint()})"

    override fun Ast.Term.Fold<C>.run(i: Unit) = "fold ${term.prettyPrint()}"

    override fun Ast.Term.Unfold<C>.run(i: Unit) = "unfold ${term.prettyPrint()}"

    override fun Ast.Term.Hole<C>.run(i: Unit): String = "?$value" + (ref.value?.let { "/(${it.prettyPrint()})" } ?: "")
}
