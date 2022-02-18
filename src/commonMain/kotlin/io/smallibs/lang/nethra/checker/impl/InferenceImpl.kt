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
// Stupid version returning a term
//

class InferenceImpl<C>(
    private val checker: Checker<C>,
    private val congruence: Congruence<C> = Congruence(),
    private val substitution: Substitution<C> = Substitution(),
    private val builder: Builder<C> = Builder(),
    private val printer: Printer<C> = Printer(),
) : Interpret<C, Gamma<C>, Term<C>>, Inference<C>, Builder<C> by builder, Congruence<C> by congruence,
    Checker<C> by checker, Printer<C> by printer, Substitution<C> by substitution {

    override fun Gamma<C>.infer(term: Term<C>): Term<C> =
        println("[↓]${this.prettyPrint()} ⊢ ${term.prettyPrint()} : ?").let {
            val r = term.run(this)
            println("[↓]${this.prettyPrint()} ⊢ ${term.prettyPrint()} : ${r.prettyPrint()}")
            r
        }

    /**
     * Interpret implemented
     */

    //
    // ----------------------
    // Γ ⊢ Type_i : Type_i+1
    override fun Term.Type<C>.run(i: Gamma<C>): Term<C> =
        type(level + 1)

    //
    // ------------------
    // Γ ⊢ data(n:T) : T
    override fun Term.Data<C>.run(i: Gamma<C>): Term<C> =
        type

    //
    // -----------------
    // Γ, x : T ⊢ x : T
    override fun Term.Id<C>.run(i: Gamma<C>): Term<C> =
        i.get(value) ?: throw Exception("unbound variable $value")

    // l ∈ int          l ∈ char
    // ------------     -------------
    // Γ ⊢ l : int     Γ ⊢ l : char
    override fun Term.Lit<C>.run(i: Gamma<C>): Term<C> =
        when (this.literal) {
            is Term.Literal.IntLit -> data("int", type())
            is Term.Literal.CharLit -> data("char", type())
        }

    // Γ ⊢ M : T   Γ, x : M ⊢ N : T
    // ------------------------------
    // Γ ⊢ Π(x:M).N : T
    override fun Term.Pi<C>.run(i: Gamma<C>): Term<C> =
        i.set(n, bound).infer(body)

    // Γ, x : A ⊢ B : T
    // ---------------------- // Cannot infer
    // Γ ⊢ λ(x).B : Π(x:A).T
    override fun Term.Lambda<C>.run(i: Gamma<C>): Term<C> =
        hole(newVariable()).let { hole ->
            pi(n, hole, i.set(hole.value, hole).infer(body.substitute(n, hole)), implicit)
        }

    // Γ ⊢ f : Π(x:M).N   Γ ⊢ e : M      Γ ⊢ f : Π{x:M}.N   Γ ⊢ e : M      Γ ⊢ f : Π{x:M}.N   Γ, v:M ⊢ f {v} e : N
    // ------------------------------      ------------------------------      -----------------------------------------
    // Γ ⊢ f e : N[x=e]                   Γ ⊢ f {e} : N[x=e]                 Γ ⊢ f e : N
    override fun Term.Apply<C>.run(i: Gamma<C>): Term<C> =
        when (val type = i.infer(abstraction)) {
            is Term.Pi ->
                if (implicit == type.implicit) {
                    if (i.check(argument, type.bound)) {
                        type.body.substitute(type.n, argument)
                    } else {
                        throw Exception("${argument.prettyPrint()} is not a ${type.bound.prettyPrint()}")
                    }
                } else if (type.implicit) {
                    val v = substitution.newVariable()
                    val t = apply(apply(abstraction, hole(v), true), argument)
                    if (i.set(v, type.bound).check(t, type.body)) {
                        type.body
                    } else {
                        throw Exception("${t.prettyPrint()} is not a ${type.prettyPrint()}")
                    }
                } else {
                    throw Exception("${this.prettyPrint()} is not a ${type.prettyPrint()}")
                }
            else -> throw Exception("Cannot infer type for ${this.prettyPrint()}")
        }

    // Γ ⊢ A : T   Γ,x : A ⊢ B : T
    // -----------------------------
    // Γ ⊢ Σ(x:A).B : T
    override fun Term.Sigma<C>.run(i: Gamma<C>): Term<C> =
        i.set(n, bound).infer(body)

    // Γ ⊢ A : M   Γ ⊢ B : N[x=A]
    // ----------------------------
    // Γ ⊢ A,B : Σ(x:M).N
    override fun Term.Couple<C>.run(i: Gamma<C>): Term<C> =
        sigma("_", i.infer(lhd), i.infer(rhd))

    // Γ ⊢ p : Σ(x:M).N
    // -----------------
    // Γ ⊢ fst p : M
    override fun Term.Fst<C>.run(i: Gamma<C>): Term<C> =
        when (val type = i.infer(term)) {
            is Term.Sigma -> type.bound
            else -> throw Exception("${this.prettyPrint()} not a dependant pair")
        }

    // Γ ⊢ p : Σ(x:M).N
    // -----------------------
    // Γ ⊢ snd p : N[x=fst p]
    override fun Term.Snd<C>.run(i: Gamma<C>): Term<C> =
        when (val type = i.infer(term)) {
            is Term.Sigma -> type.body.substitute(type.n, fst(term))
            else -> throw Exception("${this.prettyPrint()} not a dependant pair")
        }

    // Γ ⊢ A : T   Γ ⊢ B : T
    // -----------------------
    // Γ ⊢ A | B : T
    override fun Term.Disjunction<C>.run(i: Gamma<C>): Term<C> =
        or(i.infer(lhd), i.infer(rhd))

    // Γ ⊢ A : M
    // ------------------
    // Γ ⊢ inl A : M + N
    override fun Term.Inl<C>.run(i: Gamma<C>): Term<C> =
        or(i.infer(term), hole(newVariable()))

    // Γ ⊢ A : N
    // ------------------
    // Γ ⊢ inr A : M + N
    override fun Term.Inr<C>.run(i: Gamma<C>): Term<C> =
        or(hole(newVariable()), i.infer(term))

    // Γ ⊢ a : A + B   Γ ⊢ l : A -> C   Γ ⊢ r : B -> C
    // --------------------------------------------------
    // Γ ⊢ case a l r : C
    override fun Term.Case<C>.run(i: Gamma<C>): Term<C> =
        when (val type = i.infer(term)) {
            is Term.Disjunction -> {
                hole(newVariable()).let { hole ->
                    if (i.check(left, type.lhd arrow hole) && i.check(right, type.rhd arrow hole)) {
                        hole
                    } else {
                        throw Exception("${this.prettyPrint()} is not a ${type.prettyPrint()}")
                    }
                }
            }
            else -> throw Exception("Cannot infer type for ${this.prettyPrint()}")
        }

    // Γ,x : T ⊢ A : T
    // -----------------
    // Γ ⊢ rec(x).A : T
    override
    fun Term.Rec<C>.run(i: Gamma<C>): Term<C> =
        hole(newVariable()).let { hole ->
            i.set(hole.value, hole).check(body, hole)
            hole
        }

    // Γ ⊢ A : N[x=rec(x).N]
    // --------------------------------
    // Γ ⊢ fold(rec(x).N) A : rec(x).N
    override fun Term.Fold<C>.run(i: Gamma<C>): Term<C> =
        if (i.check(term, type.body.substitute(type.self, type))) {
            type
        } else {
            throw Exception("Cannot infer type for ${this.prettyPrint()}")
        }

    // Γ ⊢ A : rec(x).N
    // ---------------------------------------
    // Γ ⊢ unfold(rec(x).N) A : N[x=rec(x).N]
    override fun Term.Unfold<C>.run(i: Gamma<C>): Term<C> =
        if (i.check(term, type)) {
            type.body.substitute(type.self, type)
        } else {
            throw Exception("Cannot infer type for ${this.prettyPrint()}")
        }

    // Γ ⊢ x : T
    // --------------
    // Γ ⊢ (x ∈ T) : T
    override fun Term.Inhabit<C>.run(i: Gamma<C>): Term<C> =
        if (i.check(term, type)) {
            type
        } else {
            throw Exception("${term.prettyPrint()} is not a ${type.prettyPrint()}")
        }

    //
    // -----------------
    // Γ, x : T ⊢ x : T
    override fun Term.Hole<C>.run(i: Gamma<C>): Term<C> =
        i.get(value) ?: throw Exception("unbound variable $value")
}