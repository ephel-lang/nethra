package io.smallibs.lang.nethra.stages.s03_Checker.internal.impl

import io.smallibs.lang.nethra.ast.Ast
import io.smallibs.lang.nethra.ast.Ast.Term.Companion.ANON
import io.smallibs.lang.nethra.ast.Ast.Term.Type
import io.smallibs.lang.nethra.ast.Builder
import io.smallibs.lang.nethra.ast.Congruence
import io.smallibs.lang.nethra.ast.Printer
import io.smallibs.lang.nethra.ast.Reducer
import io.smallibs.lang.nethra.ast.Substitution
import io.smallibs.lang.nethra.ast.Visitor
import io.smallibs.lang.nethra.stages.errors.CompilationException
import io.smallibs.lang.nethra.stages.s03_Checker.internal.Bindings
import io.smallibs.lang.nethra.stages.s03_Checker.internal.Checker
import io.smallibs.lang.nethra.stages.s03_Checker.internal.Inference

//
// Stupid version returning a simple Bool
//

class CheckerImpl<C>(
    private val inferenceGenerator: (Checker<C>) -> Inference<C> = inferenceProvider(),
    private val congruence: Congruence<C> = Congruence(),
    private val substitution: Substitution<C> = Substitution(),
    private val builder: Builder<C> = Builder(),
    private val printer: Printer<C> = Printer(),
    private val reducer: Reducer<C> = Reducer(),
) : Visitor<C, CheckerImpl.Context<C>, Boolean>, Checker<C>, Builder<C> by builder, Congruence<C> by congruence,
    Printer<C> by printer, Substitution<C> by substitution, Reducer<C> by reducer {

    data class Context<C>(
        val gamma: Bindings<C>,
        val type: Ast.Term<C>,
    )

    override fun Bindings<C>.check(
        term: Ast.Term<C>,
        type: Ast.Term<C>,
    ) = reduce(type).let { type ->
        println("[↑]${this.prettyPrint()} ⊢ ${term.prettyPrint()} : ${type.prettyPrint()} / ?").let {
            // Unit.let {
            val r = term.run(Context(this, type)) || throw CompilationException.CannotCheck(term, type)
            println("[↑]${this.prettyPrint()} ⊢ ${term.prettyPrint()} : ${type.prettyPrint()} / $r")
            r
        }
    }

    /**
     * Interpret implemented
     */

    //
    // ---------------------
    // Γ ⊢ Type_i : Type_i+1
    override fun Type<C>.run(i: Context<C>) = i.gamma.congruent(Type(level + 1), i.type)

    //
    // ----------------
    // Γ, x : T ⊢ x : T
    override fun Ast.Term.Id<C>.run(i: Context<C>) =
        i.gamma.getSignature(this@run.value)?.let { type -> i.gamma.congruent(type, i.type) }
            ?: fallback(i)

    // l ∈ int         l ∈ char          l ∈ string
    // -----------     -------------     --------------
    // Γ ⊢ l : int     Γ ⊢ l : char      Γ ⊢ l : string
    override fun Ast.Term.Lit<C>.run(i: Context<C>) = when (this.literal) {
        is Ast.Term.Literal.IntLit -> i.gamma.congruent(id("int"), i.type)
        is Ast.Term.Literal.CharLit -> i.gamma.congruent(id("char"), i.type)
        is Ast.Term.Literal.StringLit -> i.gamma.congruent(id("string"), i.type)
    }

    // Γ ⊢ M : S   Γ, x : M ⊢ N : T
    // ----------------------------
    // Γ ⊢ Π(x:M).N : T
    override fun Ast.Term.Pi<C>.run(i: Context<C>) = with(inferenceGenerator(this@CheckerImpl)) {
        i.gamma.infer(bound)
        i.gamma.setSignature(n, bound).check(body, i.type)
    }


    // Γ, x : A ⊢ B : T          Γ, x : A ⊢ B : T
    // ---------------------     ---------------------
    // Γ ⊢ λ(x).B : Π(x:A).T     Γ ⊢ λ{x}.B : Π{x:A}.T
    override fun Ast.Term.Lambda<C>.run(i: Context<C>) = when (i.type) {
        is Ast.Term.Pi -> {
            if (implicit == i.type.implicit) {
                val variable = substitution.newVariable()
                val type = i.type.body.substitute(i.type.n to id(variable))
                val body = body.substitute(n to id(variable))
                i.gamma.setSignature(variable, i.type.bound).check(body, type)
            } else {
                fallback(i)
            }
        }
        else -> fallback(i)
    }

    // Γ ⊢ f : Π(x:M).N   Γ ⊢ e : M      Γ ⊢ f : Π{x:M}.N   Γ ⊢ e : M      Γ ⊢ f : Π{x:M}.N   Γ, v:M ⊢ f {v} e : N
    // ----------------------------      ----------------------------      ---------------------------------------
    // Γ ⊢ f e : N[x=e]                  Γ ⊢ f {e} : N[x=e]                Γ ⊢ f e : N
    override fun Ast.Term.Apply<C>.run(i: Context<C>) = with(inferenceGenerator(this@CheckerImpl)) {
        when (val type = i.gamma.infer(abstraction)) {
            is Ast.Term.Pi -> if (implicit == type.implicit) {
                (i.gamma.congruent(type.body.substitute(type.n to argument),
                    i.type) || throw CompilationException.CannotCompare(abstraction,
                    i.type,
                    type.body.substitute(type.n to argument))) && i.gamma.check(argument, type.bound)
            } else if (type.implicit) {
                val v = substitution.newVariable()
                i.gamma.setSignature(v, type.bound).check(apply(apply(abstraction, hole(v), true), argument), i.type)
            } else {
                fallback(i)
            }
            else -> throw CompilationException.CannotCompare(abstraction, id("?") arrow id("?"), type)
        }
    }

    // Γ ⊢ M : S   Γ, x : M ⊢ N : T
    // ----------------------------
    // Γ ⊢ Σ(x:M).N : T
    override fun Ast.Term.Sigma<C>.run(i: Context<C>) = with(inferenceGenerator(this@CheckerImpl)) {
        i.gamma.infer(bound)
        i.gamma.setSignature(n, bound).check(body, i.type)
    }

    // Γ ⊢ A : M   Γ ⊢ B : N[x=A]
    // --------------------------
    // Γ ⊢ A,B : Σ(x:M).N
    override fun Ast.Term.Couple<C>.run(i: Context<C>) = when (i.type) {
        is Ast.Term.Sigma -> i.gamma.check(lhd, i.type.bound) && i.gamma.check(rhd,
            i.type.body.substitute(i.type.n to lhd))
        else -> fallback(i)
    }

    // Γ ⊢ p : Σ(x:M).N
    // ----------------
    // Γ ⊢ fst p : M
    override fun Ast.Term.Fst<C>.run(i: Context<C>) = with(inferenceGenerator(this@CheckerImpl)) {
        when (val type = i.gamma.infer(term)) {
            is Ast.Term.Sigma -> i.gamma.congruent(type.bound, i.type)
            else -> fallback(i)
        }
    }

    // Γ ⊢ p : Σ(x:M).N
    // ----------------------
    // Γ ⊢ snd p : N[x=fst p]
    override fun Ast.Term.Snd<C>.run(i: Context<C>) = with(inferenceGenerator(this@CheckerImpl)) {
        when (val type = i.gamma.infer(term)) {
            is Ast.Term.Sigma -> i.gamma.congruent(type.body.substitute(type.n to fst(term)), i.type)
            else -> fallback(i)
        }
    }

    // Γ ⊢ A : T   Γ ⊢ B : T
    // ---------------------
    // Γ ⊢ A + B : T
    override fun Ast.Term.Disjunction<C>.run(i: Context<C>) = i.gamma.check(lhd, i.type) && i.gamma.check(rhd, i.type)

    // Γ ⊢ A : M
    // -----------------
    // Γ ⊢ inl A : M + N
    override fun Ast.Term.Inl<C>.run(i: Context<C>) = when (i.type) {
        is Ast.Term.Disjunction -> i.gamma.check(this.term, i.type.lhd)
        else -> fallback(i)
    }

    // Γ ⊢ A : N
    // -----------------
    // Γ ⊢ inr A : M + N
    override fun Ast.Term.Inr<C>.run(i: Context<C>) = when (i.type) {
        is Ast.Term.Disjunction -> i.gamma.check(this.term, i.type.rhd)
        else -> fallback(i)
    }

    // Γ ⊢ a : A + B   Γ ⊢ l : A -> C   Γ ⊢ r : B -> C
    // -----------------------------------------------
    // Γ ⊢ case a l r : C
    override fun Ast.Term.Case<C>.run(i: Context<C>) = with(inferenceGenerator(this@CheckerImpl)) {
        when (val type = i.gamma.infer(term)) {
            is Ast.Term.Disjunction -> i.gamma.check(left, type.lhd arrow i.type) && i.gamma.check(right,
                type.rhd arrow i.type)
            else -> fallback(i)
        }
    }

    // Γ,x : T ⊢ A : T
    // ----------------
    // Γ ⊢ rec(x).A : T
    override fun Ast.Term.Rec<C>.run(i: Context<C>) = i.gamma.setSignature(self, i.type).check(body, i.type)

    // Γ ⊢ A : N[x=rec(x).N]
    // ---------------------
    // Γ ⊢ fold A : rec(x).N
    override fun Ast.Term.Fold<C>.run(i: Context<C>) = when (i.type) {
        is Ast.Term.Rec -> i.gamma.check(term, i.type.body.substitute(i.type.self to i.type))
        else -> fallback(i)
    }

    // Γ ⊢ A : rec(x).N
    // ----------------------------
    // Γ ⊢ unfold A : N[x=rec(x).N]
    override fun Ast.Term.Unfold<C>.run(i: Context<C>) = with(inferenceGenerator(this@CheckerImpl)) {
        when (val type = i.gamma.infer(this@run.term)) {
            is Ast.Term.Rec -> i.gamma.congruent(type.body.substitute(type.self to type), i.type)
            else -> fallback(i)
        }
    }

    //
    // ----------------
    // Γ, x : T ⊢ x : T
    override fun Ast.Term.Hole<C>.run(i: Context<C>) =
        i.gamma.getSignature(this@run.value)?.let { type -> i.gamma.congruent(type, i.type) } ?: false

    //
    // Fallback for implicit lambda term
    //

    // Γ ⊢ {_}.B : Π{y:A}.T
    // --------------------
    // Γ ⊢ B : Π{y:A}.T
    private fun Ast.Term<C>.fallback(i: Context<C>) = when (i.type) {
        is Ast.Term.Pi -> i.type.implicit && i.gamma.check(lambda(ANON, this, true).set(this.context), i.type)
        else -> false
    }

    /**
     * Static behavior
     */

    companion object {
        private fun <C> inferenceProvider(): (Checker<C>) -> Inference<C> = { checker ->
            Inference(object : Checker<C> {
                override fun Bindings<C>.check(
                    term: Ast.Term<C>,
                    type: Ast.Term<C>,
                ) = with(checker) {
                    this@check.check(term, type)
                }
            })
        }
    }
}

