package io.smallibs.lang.nethra.stages.s03_Checker.internal.impl

import io.smallibs.lang.nethra.ast.Ast
import io.smallibs.lang.nethra.ast.Builder
import io.smallibs.lang.nethra.ast.Congruence
import io.smallibs.lang.nethra.ast.Printer
import io.smallibs.lang.nethra.ast.Substitution
import io.smallibs.lang.nethra.ast.Visitor
import io.smallibs.lang.nethra.stages.s03_Checker.internal.Checker
import io.smallibs.lang.nethra.stages.s03_Checker.internal.Context
import io.smallibs.lang.nethra.stages.s03_Checker.internal.Inference

//
// Stupid version returning a term
//

class InferenceImpl<C>(
    private val checker: Checker<C>,
    private val congruence: Congruence<C> = Congruence(),
    private val substitution: Substitution<C> = Substitution(),
    private val builder: Builder<C> = Builder(),
    private val printer: Printer<C> = Printer(),
) : Visitor<C, Context<C>, Ast.Term<C>>, Inference<C>, Builder<C> by builder, Congruence<C> by congruence,
    Checker<C> by checker, Printer<C> by printer, Substitution<C> by substitution {

    override fun Context<C>.infer(term: Ast.Term<C>): Ast.Term<C> =
        println("[↓]${this.prettyPrint()} ⊢ ${term.prettyPrint()} : ?").let {
            val r = term.run(this)
            println("[↓]${this.prettyPrint()} ⊢ ${term.prettyPrint()} : ${r.prettyPrint()}")
            r
        }

    /**
     * Interpret implemented
     */

    //
    // ---------------------
    // Γ ⊢ Type_i : Type_i+1
    override fun Ast.Term.Type<C>.run(i: Context<C>): Ast.Term<C> =
        type(level + 1)

    //
    // -----------------
    // Γ ⊢ data(n:T) : T
    override fun Ast.Term.Data<C>.run(i: Context<C>): Ast.Term<C> =
        type

    //
    // ----------------
    // Γ, x : T ⊢ x : T
    override fun Ast.Term.Id<C>.run(i: Context<C>): Ast.Term<C> =
        i.getSignature(value) ?: throw Exception("unbound variable $value")

    // l ∈ int         l ∈ char          l ∈ string
    // -----------     -------------     -------------
    // Γ ⊢ l : int     Γ ⊢ l : char      Γ ⊢ l : string
    override fun Ast.Term.Lit<C>.run(i: Context<C>): Ast.Term<C> =
        when (this.literal) {
            is Ast.Term.Literal.IntLit -> data("int", type())
            is Ast.Term.Literal.CharLit -> data("char", type())
            is Ast.Term.Literal.StringLit -> data("string", type())
        }

    // Γ ⊢ M : T   Γ, x : M ⊢ N : T
    // ----------------------------
    // Γ ⊢ Π(x:M).N : T
    override fun Ast.Term.Pi<C>.run(i: Context<C>): Ast.Term<C> =
        i.setSignature(n, bound).infer(body)

    // Γ, x : A ⊢ B : T
    // ---------------------
    // Γ ⊢ λ(x).B : Π(x:A).T
    override fun Ast.Term.Lambda<C>.run(i: Context<C>): Ast.Term<C> =
        hole(newVariable()).let { hole ->
            pi(n, hole, i.setSignature(hole.value, hole).infer(body.substitute(n, hole)), implicit)
        }

    // Γ ⊢ f : Π(x:M).N   Γ ⊢ e : M      Γ ⊢ f : Π{x:M}.N   Γ ⊢ e : M      Γ ⊢ f : Π{x:M}.N   Γ, v:M ⊢ f {v} e : N
    // ----------------------------      ----------------------------      -----------------------------------------
    // Γ ⊢ f e : N[x=e]                  Γ ⊢ f {e} : N[x=e]                Γ ⊢ f e : N
    override fun Ast.Term.Apply<C>.run(i: Context<C>): Ast.Term<C> =
        when (val type = i.infer(abstraction)) {
            is Ast.Term.Pi ->
                if (implicit == type.implicit) {
                    if (i.check(argument, type.bound)) {
                        type.body.substitute(type.n, argument)
                    } else {
                        throw Exception("${argument.prettyPrint()} is not a ${type.bound.prettyPrint()}")
                    }
                } else if (type.implicit) {
                    val v = substitution.newVariable()
                    val t = apply(apply(abstraction, hole(v), true), argument)
                    i.setSignature(v, type.bound).infer(t)
                } else {
                    throw Exception("${this.prettyPrint()} is not a ${type.prettyPrint()}")
                }
            else -> throw Exception("Cannot infer type for ${this.prettyPrint()}")
        }

    // Γ ⊢ A : T   Γ,x : A ⊢ B : T
    // ---------------------------
    // Γ ⊢ Σ(x:A).B : T
    override fun Ast.Term.Sigma<C>.run(i: Context<C>): Ast.Term<C> =
        i.setSignature(n, bound).infer(body)

    // Γ ⊢ A : M   Γ ⊢ B : N[x=A]
    // --------------------------
    // Γ ⊢ A,B : Σ(x:M).N
    override fun Ast.Term.Couple<C>.run(i: Context<C>): Ast.Term<C> =
        sigma("_", i.infer(lhd), i.infer(rhd))

    // Γ ⊢ p : Σ(x:M).N
    // ----------------
    // Γ ⊢ fst p : M
    override fun Ast.Term.Fst<C>.run(i: Context<C>): Ast.Term<C> =
        when (val type = i.infer(term)) {
            is Ast.Term.Sigma -> type.bound
            else -> throw Exception("${this.prettyPrint()} not a dependant pair")
        }

    // Γ ⊢ p : Σ(x:M).N
    // ----------------------
    // Γ ⊢ snd p : N[x=fst p]
    override fun Ast.Term.Snd<C>.run(i: Context<C>): Ast.Term<C> =
        when (val type = i.infer(term)) {
            is Ast.Term.Sigma -> type.body.substitute(type.n, fst(term))
            else -> throw Exception("${this.prettyPrint()} not a dependant pair")
        }

    // Γ ⊢ A : T   Γ ⊢ B : T
    // ---------------------
    // Γ ⊢ A | B : T
    override fun Ast.Term.Disjunction<C>.run(i: Context<C>): Ast.Term<C> =
        or(i.infer(lhd), i.infer(rhd))

    // Γ ⊢ A : M
    // -----------------
    // Γ ⊢ inl A : M + N
    override fun Ast.Term.Inl<C>.run(i: Context<C>): Ast.Term<C> =
        or(i.infer(term), hole(newVariable()))

    // Γ ⊢ A : N
    // ------------------
    // Γ ⊢ inr A : M + N
    override fun Ast.Term.Inr<C>.run(i: Context<C>): Ast.Term<C> =
        or(hole(newVariable()), i.infer(term))

    // Γ ⊢ a : A + B   Γ ⊢ l : A -> C   Γ ⊢ r : B -> C
    // -----------------------------------------------
    // Γ ⊢ case a l r : C
    override fun Ast.Term.Case<C>.run(i: Context<C>): Ast.Term<C> =
        when (val type = i.infer(term)) {
            is Ast.Term.Disjunction -> {
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
    // ----------------
    // Γ ⊢ rec(x).A : T
    override
    fun Ast.Term.Rec<C>.run(i: Context<C>): Ast.Term<C> =
        hole(newVariable()).let { hole ->
            i.setSignature(hole.value, hole).check(body, hole)
            hole
        }

    // Γ ⊢ A : N[x=rec(x).N]
    // -------------------------------
    // Γ ⊢ fold(rec(x).N) A : rec(x).N
    override fun Ast.Term.Fold<C>.run(i: Context<C>): Ast.Term<C> =
        if (i.check(term, type.body.substitute(type.self, type))) {
            type
        } else {
            throw Exception("Cannot infer type for ${this.prettyPrint()}")
        }

    // Γ ⊢ A : rec(x).N
    // --------------------------------------
    // Γ ⊢ unfold(rec(x).N) A : N[x=rec(x).N]
    override fun Ast.Term.Unfold<C>.run(i: Context<C>): Ast.Term<C> =
        if (i.check(term, type)) {
            type.body.substitute(type.self, type)
        } else {
            throw Exception("Cannot infer type for ${this.prettyPrint()}")
        }

    // Γ ⊢ x : T
    // ---------------
    // Γ ⊢ (x ∈ T) : T
    override fun Ast.Term.Inhabit<C>.run(i: Context<C>): Ast.Term<C> =
        if (i.check(term, type)) {
            type
        } else {
            throw Exception("${term.prettyPrint()} is not a ${type.prettyPrint()}")
        }

    //
    // ----------------
    // Γ, x : T ⊢ x : T
    override fun Ast.Term.Hole<C>.run(i: Context<C>): Ast.Term<C> =
        i.getSignature(value) ?: throw Exception("unbound variable $value")
}