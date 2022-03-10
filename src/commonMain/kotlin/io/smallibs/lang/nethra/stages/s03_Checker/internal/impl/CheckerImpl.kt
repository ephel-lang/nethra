package io.smallibs.lang.nethra.stages.s03_Checker.internal.impl

import io.smallibs.lang.nethra.ast.Ast.Term
import io.smallibs.lang.nethra.ast.Ast.Term.Apply
import io.smallibs.lang.nethra.ast.Ast.Term.Case
import io.smallibs.lang.nethra.ast.Ast.Term.Companion.ANON
import io.smallibs.lang.nethra.ast.Ast.Term.Couple
import io.smallibs.lang.nethra.ast.Ast.Term.Disjunction
import io.smallibs.lang.nethra.ast.Ast.Term.Fold
import io.smallibs.lang.nethra.ast.Ast.Term.Fst
import io.smallibs.lang.nethra.ast.Ast.Term.Hole
import io.smallibs.lang.nethra.ast.Ast.Term.Id
import io.smallibs.lang.nethra.ast.Ast.Term.Inl
import io.smallibs.lang.nethra.ast.Ast.Term.Inr
import io.smallibs.lang.nethra.ast.Ast.Term.Lambda
import io.smallibs.lang.nethra.ast.Ast.Term.Lit
import io.smallibs.lang.nethra.ast.Ast.Term.Pi
import io.smallibs.lang.nethra.ast.Ast.Term.Rec
import io.smallibs.lang.nethra.ast.Ast.Term.Sigma
import io.smallibs.lang.nethra.ast.Ast.Term.Snd
import io.smallibs.lang.nethra.ast.Ast.Term.Type
import io.smallibs.lang.nethra.ast.Ast.Term.Unfold
import io.smallibs.lang.nethra.ast.Builder
import io.smallibs.lang.nethra.ast.Congruence
import io.smallibs.lang.nethra.ast.Printer
import io.smallibs.lang.nethra.ast.Reducer
import io.smallibs.lang.nethra.ast.Substitution
import io.smallibs.lang.nethra.ast.Visitor
import io.smallibs.lang.nethra.stages.report.CompilationException
import io.smallibs.lang.nethra.stages.s03_Checker.internal.Bindings
import io.smallibs.lang.nethra.stages.s03_Checker.internal.Checker
import io.smallibs.lang.nethra.stages.s03_Checker.internal.Inference
import io.smallibs.lang.nethra.stages.s03_Checker.internal.Proof
import io.smallibs.lang.nethra.stages.s03_Checker.internal.Proof.Congruent
import io.smallibs.lang.nethra.stages.s03_Checker.internal.Proof.Failure
import io.smallibs.lang.nethra.stages.s03_Checker.internal.Proof.Step

class CheckerImpl<C>(
    private val inferenceGenerator: (Checker<C>) -> Inference<C> = inferenceProvider(),
    private val congruence: Congruence<C> = Congruence(),
    private val substitution: Substitution<C> = Substitution(),
    private val builder: Builder<C> = Builder(),
    private val printer: Printer<C> = Printer(),
    private val reducer: Reducer<C> = Reducer(),
) : Visitor<C, CheckerImpl.Context<C>, List<Proof<C>>>, Checker<C>, Builder<C> by builder, Congruence<C> by congruence,
    Printer<C> by printer, Substitution<C> by substitution, Reducer<C> by reducer {

    data class Context<C>(
        val gamma: Bindings<C>,
        val type: Term<C>,
    )

    override fun Bindings<C>.check(
        term: Term<C>,
        type: Term<C>,
    ): Proof<C> = reduce(type).let { reducedType ->
        val i = Context(this, reducedType)
        val r = term.run(i).let { r ->
            if (r.all { it.success() }) {
                Step(Proof.Check(i.gamma, term, reducedType), r)
            } else {
                val fallback = term.fallback(i)
                if (fallback.all { it.success() }) {
                    Step(Proof.Check(i.gamma, term, reducedType), fallback)
                } else {
                    Step(Proof.Check(i.gamma, term, reducedType), r)
                }
            }
        }
        r
    }

    /**
     * Interpret implemented
     */

    //
    // ---------------------
    // Γ ⊢ Type_i : Type_i+1
    override fun Type<C>.run(i: Context<C>): List<Proof<C>> =
        if (i.gamma.congruent(type(level + 1), i.type)) {
            listOf(Step(Congruent(i.gamma, this, type(level + 1), i.type)))
        } else {
            listOf(Step(Congruent(i.gamma, this, type(level + 1), i.type), listOf(Failure())))
        }

    //
    // ----------------
    // Γ, x : T ⊢ x : T
    override fun Id<C>.run(i: Context<C>): List<Proof<C>> =
        i.gamma.getSignature(this@run.value)?.let { type ->
            if (i.gamma.congruent(type, i.type)) {
                listOf(Step(Congruent(i.gamma, this@run, i.type, type)))
            } else {
                listOf(Step(Congruent(i.gamma, this@run, i.type, type), listOf(Failure())))
            }
        } ?: listOf(Failure(CompilationException.Unbound(this)))

    // l ∈ int         l ∈ char          l ∈ string
    // -----------     -------------     --------------
    // Γ ⊢ l : int     Γ ⊢ l : char      Γ ⊢ l : string
    override fun Lit<C>.run(i: Context<C>): List<Proof<C>> = with(inferenceGenerator(this@CheckerImpl)) {
        i.gamma.infer(this@run).let { infer ->
            infer.first?.let { type ->
                if (i.gamma.congruent(type, i.type)) {
                    listOf(infer.second, Step(Congruent(i.gamma, this@run, i.type, type)))
                } else {
                    listOf(infer.second, Step(Congruent(i.gamma, this@run, i.type, type), listOf(Failure())))
                }
            } ?: listOf(infer.second, Failure())
        }
    }

    // Γ ⊢ M : S   Γ, x : M ⊢ N : T
    // ----------------------------
    // Γ ⊢ Π(x:M).N : T
    override fun Pi<C>.run(i: Context<C>): List<Proof<C>> = with(inferenceGenerator(this@CheckerImpl)) {
        i.gamma.infer(bound).let { infer ->
            listOf(infer.second, i.gamma.setSignature(n, bound).check(body, i.type))
        }
    }

    // Γ, x : A ⊢ B : T          Γ, x : A ⊢ B : T
    // ---------------------     ---------------------
    // Γ ⊢ λ(x).B : Π(x:A).T     Γ ⊢ λ{x}.B : Π{x:A}.T
    override fun Lambda<C>.run(i: Context<C>): List<Proof<C>> = when (i.type) {
        is Pi -> {
            if (implicit == i.type.implicit) {
                val variable = substitution.newVariable()
                val type = i.type.body.substitute(i.type.n to id(variable))
                val body = body.substitute(n to id(variable))
                listOf(i.gamma.setSignature(variable, i.type.bound).check(body, type))
            } else {
                listOf(Failure())
            }
        }
        else -> listOf(Failure())
    }

    // Γ ⊢ f : Π(x:M).N   Γ ⊢ e : M      Γ ⊢ f : Π{x:M}.N   Γ ⊢ e : M      Γ ⊢ f : Π{x:M}.N   Γ, v:M ⊢ f {v} e : N
    // ----------------------------      ----------------------------      ---------------------------------------
    // Γ ⊢ f e : N[x=e]                  Γ ⊢ f {e} : N[x=e]                Γ ⊢ f e : N
    override fun Apply<C>.run(i: Context<C>): List<Proof<C>> = with(inferenceGenerator(this@CheckerImpl)) {
        val infer = i.gamma.infer(abstraction)
        when (val type = infer.first) {
            is Pi -> if (implicit == type.implicit) {
                val lhd = type.body.substitute(type.n to argument)
                if (i.gamma.congruent(lhd, i.type)) {
                    listOf(i.gamma.check(argument, type.bound))
                } else {
                    listOf(infer.second, Step(Congruent(i.gamma, argument, lhd, i.type), listOf(Failure())))
                }
            } else if (type.implicit) {
                val variable = substitution.newVariable()
                val gamma = i.gamma.setSignature(variable, type.bound)
                listOf(gamma.check(apply(apply(abstraction, hole(variable), true), argument), i.type))
            } else {
                listOf(infer.second, Failure())
            }
            else -> listOf(infer.second, Failure())
        }
    }

    // Γ ⊢ M : S   Γ, x : M ⊢ N : T
    // ----------------------------
    // Γ ⊢ Σ(x:M).N : T
    override fun Sigma<C>.run(i: Context<C>): List<Proof<C>> = with(inferenceGenerator(this@CheckerImpl)) {
        i.gamma.infer(bound) // TODO
        listOf(i.gamma.setSignature(n, bound).check(body, i.type))
    }

    // Γ ⊢ A : M   Γ ⊢ B : N[x=A]
    // --------------------------
    // Γ ⊢ A,B : Σ(x:M).N
    override fun Couple<C>.run(i: Context<C>): List<Proof<C>> = when (i.type) {
        is Sigma -> {
            listOf(i.gamma.check(lhd, i.type.bound), i.gamma.check(rhd, i.type.body.substitute(i.type.n to lhd)))
        }
        else -> listOf(Failure())
    }

    // Γ ⊢ p : Σ(x:M).N
    // ----------------
    // Γ ⊢ fst p : M
    override fun Fst<C>.run(i: Context<C>): List<Proof<C>> = with(inferenceGenerator(this@CheckerImpl)) {
        val infer = i.gamma.infer(term)
        when (val type = infer.first) {
            is Sigma ->
                if (i.gamma.congruent(type.bound, i.type)) {
                    listOf(infer.second)
                } else {
                    listOf(Step(Congruent(i.gamma, term, type.bound, i.type), listOf(infer.second, Failure())))
                }
            else -> listOf(infer.second, Failure())
        }
    }

    // Γ ⊢ p : Σ(x:M).N
    // ----------------------
    // Γ ⊢ snd p : N[x=fst p]
    override fun Snd<C>.run(i: Context<C>): List<Proof<C>> = with(inferenceGenerator(this@CheckerImpl)) {
        val infer = i.gamma.infer(term)
        when (val type = infer.first) {
            is Sigma -> {
                val lhd = type.body.substitute(type.n to fst(term))
                if (i.gamma.congruent(lhd, i.type)) {
                    listOf(infer.second, Step(Congruent(i.gamma, this@run, lhd, i.type)))
                } else {
                    listOf(infer.second, Step(Congruent(i.gamma, this@run, lhd, i.type), listOf(Failure())))
                }
            }
            else -> listOf(infer.second, Failure())
        }
    }

    // Γ ⊢ A : T   Γ ⊢ B : T
    // ---------------------
    // Γ ⊢ A + B : T
    override fun Disjunction<C>.run(i: Context<C>): List<Proof<C>> =
        listOf(i.gamma.check(lhd, i.type), i.gamma.check(rhd, i.type))

    // Γ ⊢ A : M
    // -----------------
    // Γ ⊢ inl A : M + N
    override fun Inl<C>.run(i: Context<C>): List<Proof<C>> = when (i.type) {
        is Disjunction -> listOf(i.gamma.check(this.term, i.type.lhd))
        else -> listOf(Failure())
    }

    // Γ ⊢ A : N
    // -----------------
    // Γ ⊢ inr A : M + N
    override fun Inr<C>.run(i: Context<C>): List<Proof<C>> = when (i.type) {
        is Disjunction -> listOf(i.gamma.check(this.term, i.type.rhd))
        else -> listOf(Failure())
    }

    // Γ ⊢ a : A + B   Γ ⊢ l : A -> C   Γ ⊢ r : B -> C
    // -----------------------------------------------
    // Γ ⊢ case a l r : C
    override fun Case<C>.run(i: Context<C>): List<Proof<C>> = with(inferenceGenerator(this@CheckerImpl)) {
        val infer = i.gamma.infer(term)
        when (val type = infer.first) {
            is Disjunction ->
                listOf(infer.second,
                    i.gamma.check(left, type.lhd arrow i.type),
                    i.gamma.check(right, type.rhd arrow i.type))
            else -> listOf(infer.second, Failure())
        }
    }

    // Γ,x : T ⊢ A : T
    // ----------------
    // Γ ⊢ rec(x).A : T
    override fun Rec<C>.run(i: Context<C>): List<Proof<C>> =
        listOf(i.gamma.setSignature(self, i.type).check(body, i.type))

    // Γ ⊢ A : N[x=rec(x).N]
    // ---------------------
    // Γ ⊢ fold A : rec(x).N
    override fun Fold<C>.run(i: Context<C>): List<Proof<C>> = when (i.type) {
        is Rec -> listOf(i.gamma.check(term, i.type.body.substitute(i.type.self to i.type)))
        else -> listOf(Failure())
    }

    // Γ ⊢ A : rec(x).N
    // ----------------------------
    // Γ ⊢ unfold A : N[x=rec(x).N]
    override fun Unfold<C>.run(i: Context<C>): List<Proof<C>> = with(inferenceGenerator(this@CheckerImpl)) {
        val infer = i.gamma.infer(this@run.term)
        when (val type = infer.first) {
            is Rec -> {
                val lhd = type.body.substitute(type.self to type)
                if (i.gamma.congruent(lhd, i.type)) {
                    listOf(infer.second, Step(Congruent(i.gamma, this@run, lhd, i.type)))
                } else {
                    listOf(infer.second, Step(Congruent(i.gamma, this@run, lhd, i.type), listOf(Failure())))
                }
            }
            else -> listOf(infer.second, Failure())
        }
    }

    //
    // ----------------
    // Γ, x : T ⊢ x : T
    override fun Hole<C>.run(i: Context<C>): List<Proof<C>> =
        (this.ref.value ?: i.gamma.getSignature(this.value))?.let { type ->
            if (i.gamma.congruent(type, i.type)) {
                listOf(Step(Congruent(i.gamma, this, type, i.type)))
            } else {
                listOf(Step(Congruent(i.gamma, this, type, i.type), listOf(Failure())))
            }
        } ?: listOf(Failure(CompilationException.Unbound(this)))

    //
    // Fallback for implicit lambda term
    //

    // Γ ⊢ {_}.B : Π{y:A}.T    Γ ⊢ t : Type_i
    // --------------------    ------------------
    // Γ ⊢ B : Π{y:A}.T        Γ ⊢ t : Type_{i+1}
    private fun Term<C>.fallback(i: Context<C>): List<Proof<C>> = when (i.type) {
        is Pi -> if (i.type.implicit) {
            if (this is Lambda<C> && this.implicit) {
                listOf(Failure())
            } else {
                listOf(i.gamma.check(lambda(ANON, this, true).set(this.context), i.type))
            }
        } else {
            listOf(Failure())
        }
        is Type -> if (i.type.level > 0) {
            listOf(i.gamma.check(this, type(i.type.level - 1)))
        } else {
            listOf(Failure())
        }
        else -> listOf(Failure())
    }

    /**
     * Static behavior
     */

    companion object {
        private fun <C> inferenceProvider(): (Checker<C>) -> Inference<C> = { checker ->
            Inference(object : Checker<C> {
                override fun Bindings<C>.check(
                    term: Term<C>,
                    type: Term<C>,
                ) = with(checker) {
                    this@check.check(term, type)
                }
            })
        }
    }
}

