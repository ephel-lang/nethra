package io.smallibs.lang.nethra.ast.impl

import io.smallibs.lang.nethra.ast.Interpret
import io.smallibs.lang.nethra.ast.Printer
import io.smallibs.lang.nethra.ast.Term

class PrinterImpl<C> : Printer<C>, Interpret<C, Unit, String> {

    override fun Term<C>.prettyPrint(): String = this.run(Unit)

    /**
     * Interpret implementation
     */

    override fun Term.Type<C>.run(i: Unit) = "Type_$level"

    override fun Term.Data<C>.run(i: Unit) = "$value"
    override fun Term.Id<C>.run(i: Unit) = value

    override fun Term.Lit<C>.run(i: Unit) =
        when (this.literal) {
            is Term.Literal.IntLit -> this.literal.value.toString()
            is Term.Literal.CharLit -> "'${this.literal.value}'"
        }

    override fun Term.Pi<C>.run(i: Unit) =
        when (implicit) {
            true -> "Π{${n}:${bound.prettyPrint()}}.(${body.prettyPrint()})"
            false ->
                if (n == "_")
                    "${bound.prettyPrint()} -> ${body.prettyPrint()}"
                else
                    "Π(${n}:${bound.prettyPrint()}).(${body.prettyPrint()})"
        }

    override fun Term.Lambda<C>.run(i: Unit) =
        when (implicit) {
            true -> "λ{${n}}.(${body.prettyPrint()})"
            false -> "λ(${n}).(${body.prettyPrint()})"
        }

    override fun Term.Apply<C>.run(i: Unit) =
        when (implicit) {
            true -> "${abstraction.prettyPrint()} ({${argument.prettyPrint()}})"
            false -> "${abstraction.prettyPrint()} (${argument.prettyPrint()})"
        }

    override fun Term.Sigma<C>.run(i: Unit) = "Σ(${n}:${bound.prettyPrint()}).(${body.prettyPrint()})"

    override fun Term.Couple<C>.run(i: Unit) = "(${lhd.prettyPrint()},${rhd.prettyPrint()})"

    override fun Term.Fst<C>.run(i: Unit): String = "fst ${term.prettyPrint()}"

    override fun Term.Snd<C>.run(i: Unit): String = "snd ${term.prettyPrint()}"

    override fun Term.Disjunction<C>.run(i: Unit) = "${lhd.prettyPrint()} + ${rhd.prettyPrint()}"

    override fun Term.Inl<C>.run(i: Unit) = "inl ${term.prettyPrint()}"

    override fun Term.Inr<C>.run(i: Unit) = "inr ${term.prettyPrint()}"

    override fun Term.Case<C>.run(i: Unit)= "case ${term.prettyPrint()} ${left.prettyPrint()} ${right.prettyPrint()}"

    override fun Term.Rec<C>.run(i: Unit) = "μ(${self}).(${body.prettyPrint()})"

    override fun Term.Fold<C>.run(i: Unit) = "fold(${type.prettyPrint()}) ${term.prettyPrint()}"

    override fun Term.Unfold<C>.run(i: Unit) = "unfold(${type.prettyPrint()}) ${term.prettyPrint()}"

    override fun Term.Inhabit<C>.run(i: Unit) = "(${term.prettyPrint()} ∈ ${type.prettyPrint()})"

    override fun Term.Hole<C>.run(i: Unit): String = "?$value" + (term?.let { "/${it.prettyPrint()}" } ?: "")
}
