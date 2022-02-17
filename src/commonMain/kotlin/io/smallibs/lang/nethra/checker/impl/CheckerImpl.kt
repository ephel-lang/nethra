package io.smallibs.lang.nethra.checker.impl

import io.smallibs.lang.nethra.ast.Builder
import io.smallibs.lang.nethra.ast.Congruence
import io.smallibs.lang.nethra.ast.Interpret
import io.smallibs.lang.nethra.ast.Printer
import io.smallibs.lang.nethra.ast.Substitution
import io.smallibs.lang.nethra.ast.Term
import io.smallibs.lang.nethra.checker.Checker
import io.smallibs.lang.nethra.checker.Gamma
import io.smallibs.lang.nethra.checker.Inference

//
// Stupid version returning a simple Bool
//

class CheckerImpl<C>(
    private val inference: (Checker<C>) -> Inference<C> = inferenceProvider(),
    private val congruence: Congruence<C> = Congruence(),
    private val substitution: Substitution<C> = Substitution(),
    private val builder: Builder<C> = Builder(),
    private val printer: Printer<C> = Printer(),
) : Interpret<C, CheckerImpl.Context<C>, Boolean>, Checker<C>, Builder<C> by builder, Congruence<C> by congruence,
    Printer<C> by printer, Substitution<C> by substitution {

    data class Context<C>(val gamma: Gamma<C>, val type: Term<C>)

    override fun Gamma<C>.check(term: Term<C>, type: Term<C>) =
        println("[↑]${this.prettyPrint()} |- ${term.prettyPrint()} : ${type.prettyPrint()} / ?").let {
            val r = try {
                term.run(Context(this, type))
            } catch (_: Exception) {
                false
            }
            println("[↑]${this.prettyPrint()} |- ${term.prettyPrint()} : ${type.prettyPrint()} / $r")
            r
        }

    /**
     * Interpret implemented
     */

    //
    // ----------------------
    // Γ |- Type_i : Type_i+1
    override fun Term.Type<C>.run(i: Context<C>) = Term.Type<C>(level + 1) compatibleWith i.type

    //
    // ------------------
    // Γ |- data(n:T) : T
    override fun Term.Data<C>.run(i: Context<C>) = this@run.type compatibleWith i.type

    //
    // -----------------
    // Γ, x : T |- x : T
    override fun Term.Id<C>.run(i: Context<C>) =
        i.gamma.get(this@run.value)?.let { type -> type compatibleWith i.type } ?: false

    // l ∈ int          l ∈ char
    // ------------     -------------
    // Γ |- l : int     Γ |- l : char
    override fun Term.Lit<C>.run(i: Context<C>) = when (this.literal) {
        is Term.Literal.IntLit -> data("int", type()) compatibleWith i.type
        is Term.Literal.CharLit -> data("char", type()) compatibleWith i.type
    }

    // Γ, x : M |- N : T
    // -----------------
    // Γ |- Π(x:M).N : T
    override fun Term.Pi<C>.run(i: Context<C>) = when (i.type) {
        is Term.Type -> i.gamma.set(n, bound).check(body, i.type)
        else -> false
    }

    // Γ, x : A |- B : T          Γ, x : A |- B : T
    // ----------------------     ----------------------
    // Γ |- λ(x).B : Π(x:A).T     Γ |- λ{x}.B : Π{x:A}.T
    override fun Term.Lambda<C>.run(i: Context<C>) = when (i.type) {
        is Term.Pi -> {
            val variable = substitution.newVariable()
            val body = body.substitute(n, id(variable))
            val type = i.type.body.substitute(i.type.n, id(variable))
            implicit == i.type.implicit && i.gamma.set(variable, i.type.bound).check(body, type)
        }
        else -> false
    }

    // Γ |- f : Π(x:M).N   Γ |- e : M      Γ |- f : Π{x:M}.N   Γ |- e : M      Γ |- f : Π{x:M}.N   Γ, v:M |- f {v} e : N
    // ------------------------------      ------------------------------      -----------------------------------------
    // Γ |- f e : N[x=e]                   Γ |- f {e} : N[x=e]                 Γ |- f e : N
    override fun Term.Apply<C>.run(i: Context<C>) = with(inference(this@CheckerImpl)) {
        when (val type = i.gamma.infer(abstraction)) {
            is Term.Pi -> if (implicit == type.implicit) {
                type.body.substitute(type.n, argument) compatibleWith i.type && i.gamma.check(argument, type.bound)
            } else if (type.implicit) {
                val v = substitution.newVariable()
                i.gamma.set(v, type.bound).check(apply(apply(abstraction, hole(v), true), argument), i.type)
            } else {
                false
            }
            else -> false
        }
    }

    // Γ,x : A |- B : T
    // -----------------
    // Γ |- Σ(x:A).B : T
    override fun Term.Sigma<C>.run(i: Context<C>) = i.gamma.set(n, bound).check(body, i.type)

    // Γ |- A : M   Γ |- B : N[x=A]
    // ----------------------------
    // Γ |- A,B : Σ(x:M).N
    override fun Term.Couple<C>.run(i: Context<C>) = when (i.type) {
        is Term.Sigma -> i.gamma.check(lhd, i.type.bound) && i.gamma.check(rhd, i.type.body.substitute(i.type.n, lhd))
        else -> false
    }

    // Γ |- p : Σ(x:M).N
    // -----------------
    // Γ |- fst p : M
    override fun Term.Fst<C>.run(i: Context<C>) = with(inference(this@CheckerImpl)) {
        when (val type = i.gamma.infer(term)) {
            is Term.Sigma -> type.bound compatibleWith i.type
            else -> false
        }
    }

    // Γ |- p : Σ(x:M).N
    // -----------------------
    // Γ |- snd p : N[x=fst p]
    override fun Term.Snd<C>.run(i: Context<C>) = with(inference(this@CheckerImpl)) {
        when (val type = i.gamma.infer(term)) {
            is Term.Sigma -> type.body.substitute(type.n, fst(term)) compatibleWith i.type
            else -> false
        }
    }

    // Γ |- A : T   Γ |- B : T
    // -----------------------
    // Γ |- A + B : T
    override fun Term.Disjunction<C>.run(i: Context<C>) = i.gamma.check(lhd, i.type) && i.gamma.check(rhd, i.type)

    // Γ |- A : M
    // ------------------
    // Γ |- inl A : M + N
    override fun Term.Inl<C>.run(i: Context<C>) = when (i.type) {
        is Term.Disjunction -> i.gamma.check(this.term, i.type.lhd)
        else -> false
    }

    // Γ |- A : N
    // ------------------
    // Γ |- inr A : M + N
    override fun Term.Inr<C>.run(i: Context<C>) = when (i.type) {
        is Term.Disjunction -> i.gamma.check(this.term, i.type.rhd)
        else -> false
    }

    // Γ |- a : A + B   Γ |- l : A -> C   Γ |- r : B -> C
    // --------------------------------------------------
    // Γ |- case a l r : C
    override fun Term.Case<C>.run(i: Context<C>) = with(inference(this@CheckerImpl)) {
        when (val type = i.gamma.infer(term)) {
            is Term.Disjunction -> i.gamma.check(left, type.lhd arrow i.type) && i.gamma.check(right,
                type.rhd arrow i.type)
            else -> false
        }
    }

    // Γ,x : T |- A : T
    // -----------------
    // Γ |- rec(x).A : T
    override fun Term.Rec<C>.run(i: Context<C>) = i.gamma.set(self, i.type).check(body, i.type)

    // Γ |- A : N[x=rec(x).N]
    // --------------------------------
    // Γ |- fold(rec(x).N) A : rec(x).N
    override fun Term.Fold<C>.run(i: Context<C>) =
        type compatibleWith i.type && i.gamma.check(term, type.body.substitute(type.self, i.type))

    // Γ |- A : rec(x).N
    // ---------------------------------------
    // Γ |- unfold(rec(x).N) A : N[x=rec(x).N]
    override fun Term.Unfold<C>.run(i: Context<C>) =
        type.body.substitute(type.self, type) compatibleWith i.type && i.gamma.check(term, type)

    // Γ |- x : T
    // --------------
    // Γ |- (x ∈ T) : T
    override fun Term.Inhabit<C>.run(i: Context<C>) = i.gamma.check(term, i.type) && type compatibleWith i.type

    //
    // -----------------
    // Γ, x : T |- x : T
    override fun Term.Hole<C>.run(i: Context<C>) =
        i.gamma.get(this@run.value)?.let { type -> type compatibleWith i.type } ?: false

    /**
     * Static behavior
     */

    companion object {
        private fun <C> inferenceProvider(): (Checker<C>) -> Inference<C> = { checker ->
            Inference(object : Checker<C> {
                override fun Gamma<C>.check(term: Term<C>, type: Term<C>) = with(checker) {
                    this@check.check(term, type)
                }
            })
        }
    }
}

