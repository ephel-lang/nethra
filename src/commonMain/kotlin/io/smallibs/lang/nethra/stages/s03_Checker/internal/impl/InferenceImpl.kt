package io.smallibs.lang.nethra.stages.s03_Checker.internal.impl

import io.smallibs.lang.nethra.ast.Ast
import io.smallibs.lang.nethra.ast.Ast.Term.Companion.ANON
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

class InferenceImpl<C>(
    private val checker: Checker<C>,
    private val congruence: Congruence<C> = Congruence(),
    private val substitution: Substitution<C> = Substitution(),
    private val builder: Builder<C> = Builder(),
    private val printer: Printer<C> = Printer(),
    private val reducer: Reducer<C> = Reducer(),
) : Visitor<C, Bindings<C>, Pair<Ast.Term<C>?, List<Proof<C>>>>, Inference<C>, Builder<C> by builder,
    Congruence<C> by congruence, Checker<C> by checker, Printer<C> by printer, Substitution<C> by substitution,
    Reducer<C> by reducer {

    override fun Bindings<C>.infer(term: Ast.Term<C>): Pair<Ast.Term<C>?, Proof<C>> =
        term.run(this).let { inferred -> inferred.first?.let { reduce(it) } to inferred.second }.let { inferred ->
            inferred.first to Proof.Step(Proof.Infer(this, term, inferred.first), inferred.second)
        }

    /**
     * Interpret implemented
     */

    //
    // ---------------------
    // Γ ⊢ Type_i : Type_i+1
    override fun Ast.Term.Type<C>.run(i: Bindings<C>): Pair<Ast.Term<C>?, List<Proof<C>>> = type(level + 1) to listOf()

    //
    // ----------------
    // Γ, x : T ⊢ x : T
    override fun Ast.Term.Id<C>.run(i: Bindings<C>): Pair<Ast.Term<C>?, List<Proof<C>>> =
        i.getSignature(value)?.let { it to listOf() }
            ?: (null to listOf(Proof.Failure(CompilationException.Unbound(this))))

    // l ∈ int         l ∈ char          l ∈ string
    // -----------     -------------     -------------
    // Γ ⊢ l : int     Γ ⊢ l : char      Γ ⊢ l : string
    override fun Ast.Term.Lit<C>.run(i: Bindings<C>): Pair<Ast.Term<C>?, List<Proof<C>>> = when (this.literal) {
        is Ast.Term.Literal.IntLit -> id("int") to listOf()
        is Ast.Term.Literal.CharLit -> id("char") to listOf()
        is Ast.Term.Literal.StringLit -> id("string") to listOf()
    }

    // Γ ⊢ M : S   Γ, x : M ⊢ N : T
    // ----------------------------
    // Γ ⊢ Π(x:M).N : T
    override fun Ast.Term.Pi<C>.run(i: Bindings<C>): Pair<Ast.Term<C>?, List<Proof<C>>> {
        val tbound = i.infer(bound)
        val tbody = i.setSignature(n.value, bound).infer(body)
        return tbody.first to listOf(tbound.second, tbody.second)
    }

    // Γ, x : A ⊢ B : T          Γ, x : A ⊢ B : T
    // ---------------------     ---------------------
    // Γ ⊢ λ(x).B : Π(x:A).T     Γ ⊢ λ{x}.B : Π{x:A}.T
    override fun Ast.Term.Lambda<C>.run(i: Bindings<C>): Pair<Ast.Term<C>?, List<Proof<C>>> =
        hole(newVariable()).let { hole ->
            val tbody = i.setSignature(hole.value, hole).infer(body.substitute(n to hole))
            tbody.first?.let { pi(n, hole, it, implicit) to listOf(tbody.second) } ?: (null to listOf(tbody.second))
        }

    // Γ ⊢ f : Π(x:M).N   Γ ⊢ e : M      Γ ⊢ f : Π{x:M}.N   Γ ⊢ e : M      Γ ⊢ f : Π{x:M}.N   Γ, v:M ⊢ f {v} e : N
    // ----------------------------      ----------------------------      -----------------------------------------
    // Γ ⊢ f e : N[x=e]                  Γ ⊢ f {e} : N[x=e]                Γ ⊢ f e : N
    override fun Ast.Term.Apply<C>.run(i: Bindings<C>): Pair<Ast.Term<C>?, List<Proof<C>>> =
        i.infer(abstraction).let { inference ->
            when (val type = inference.first) {
                is Ast.Term.Pi -> if (implicit == type.implicit) {
                    if (i.check(argument, type.bound).success()) {
                        type.body.substitute(type.n to argument) to listOf()
                    } else {
                        type to listOf(Proof.Failure())
                    }
                } else if (type.implicit) {
                    val v = substitution.newVariable()
                    val t = apply(apply(abstraction, hole(v), true), argument)
                    val r = i.setSignature(v, type.bound).infer(t)
                    r.first to listOf(r.second)
                } else {
                    type to listOf(Proof.Step(Proof.Check(i, this, type), listOf(Proof.Failure())))
                }
                else -> type to listOf(Proof.Failure())
            }
        }

    // Γ ⊢ M : S   Γ,x : M ⊢ N : T
    // ---------------------------
    // Γ ⊢ Σ(x:M).N : T
    override fun Ast.Term.Sigma<C>.run(i: Bindings<C>): Pair<Ast.Term<C>?, List<Proof<C>>> {
        val tbound = i.infer(bound)
        val tbody = i.setSignature(n.value, bound).infer(body)
        return tbody.first to listOf(tbound.second, tbody.second)
    }

    // Γ ⊢ A : M   Γ ⊢ B : N[x=A]
    // --------------------------
    // Γ ⊢ A,B : Σ(x:M).N
    override fun Ast.Term.Couple<C>.run(i: Bindings<C>): Pair<Ast.Term<C>?, List<Proof<C>>> {
        val bound = i.infer(lhd)
        val body = i.infer(rhd)
        return bound.first?.let { tbound ->
            body.first?.let { tbody ->
                sigma(id(ANON), tbound, tbody)
            }
        } to listOf(bound.second, body.second)
    }

    // Γ ⊢ p : Σ(x:M).N
    // ----------------
    // Γ ⊢ fst p : M
    override fun Ast.Term.Fst<C>.run(i: Bindings<C>): Pair<Ast.Term<C>?, List<Proof<C>>> =
        i.infer(term).let { infer ->
            return when (val type = infer.first) {
                is Ast.Term.Sigma -> type.bound to listOf(infer.second)
                else -> type to listOf(Proof.Failure())
            }
        }

    // Γ ⊢ p : Σ(x:M).N
    // ----------------------
    // Γ ⊢ snd p : N[x=fst p]
    override fun Ast.Term.Snd<C>.run(i: Bindings<C>): Pair<Ast.Term<C>?, List<Proof<C>>> =
        i.infer(term).let { infer ->
            when (val type = infer.first) {
                is Ast.Term.Sigma -> type.body.substitute(type.n to fst(term)) to listOf(infer.second)
                else -> type to listOf(Proof.Step(Proof.Infer(i, this, type), listOf(Proof.Failure())))
            }
        }

    // Γ ⊢ A : T   Γ ⊢ B : T
    // ---------------------
    // Γ ⊢ A | B : T
    override fun Ast.Term.Disjunction<C>.run(i: Bindings<C>): Pair<Ast.Term<C>?, List<Proof<C>>> {
        val inferlhd = i.infer(lhd)
        val inferrhd = i.infer(rhd)
        return inferlhd.first?.let { tlhd ->
            inferrhd.first?.let { trhd ->
                or(tlhd, trhd)
            }
        } to listOf(inferlhd.second, inferrhd.second)
    }

    // Γ ⊢ A : M
    // -----------------
    // Γ ⊢ inl A : M + N
    override fun Ast.Term.Inl<C>.run(i: Bindings<C>): Pair<Ast.Term<C>?, List<Proof<C>>> =
        i.infer(term).let { inferlhd ->
            inferlhd.first?.let { tlhd ->
                or(tlhd, hole(newVariable()))
            } to listOf(inferlhd.second)
        }

    // Γ ⊢ A : N
    // ------------------
    // Γ ⊢ inr A : M + N
    override fun Ast.Term.Inr<C>.run(i: Bindings<C>): Pair<Ast.Term<C>?, List<Proof<C>>> =
        i.infer(term).let { inferrhd ->
            inferrhd.first?.let { trhd ->
                or(hole(newVariable()), trhd)
            } to listOf(inferrhd.second)
        }

    // Γ ⊢ a : A + B   Γ ⊢ l : A -> C   Γ ⊢ r : B -> C
    // -----------------------------------------------
    // Γ ⊢ case a l r : C
    override fun Ast.Term.Case<C>.run(i: Bindings<C>): Pair<Ast.Term<C>?, List<Proof<C>>> {
        val infer = i.infer(term)
        return when (val type = infer.first) {
            is Ast.Term.Disjunction -> {
                hole(newVariable()).let { hole ->
                    hole to listOf(i.check(left, type.lhd arrow hole), i.check(right, type.rhd arrow hole))
                }
            }
            else -> type to listOf(Proof.Step(Proof.Infer(i, this, type), listOf(Proof.Failure())))
        }
    }

    // Γ,x : T ⊢ A : T
    // ----------------
    // Γ ⊢ rec(x).A : T
    override fun Ast.Term.Rec<C>.run(i: Bindings<C>): Pair<Ast.Term<C>?, List<Proof<C>>> =
        hole(newVariable()).let { hole ->
            hole to listOf(i.setSignature(hole.value, hole).check(body, hole))
        }

    // Γ ⊢ A : N[x=rec(x).N]
    // ---------------------
    // Γ ⊢ fold A : rec(x).N
    override fun Ast.Term.Fold<C>.run(i: Bindings<C>): Pair<Ast.Term<C>?, List<Proof<C>>> =
        null to listOf(Proof.Failure())

    // Γ ⊢ A : rec(x).N
    // ----------------------------
    // Γ ⊢ unfold A : N[x=rec(x).N]
    override fun Ast.Term.Unfold<C>.run(i: Bindings<C>): Pair<Ast.Term<C>?, List<Proof<C>>> {
        val infer = i.infer(this@run.term)
        return when (val type = infer.first) {
            is Ast.Term.Rec -> type.body.substitute(type.self to type) to listOf(infer.second)
            else -> type to listOf(Proof.Step(Proof.Infer(i, this, type), listOf(Proof.Failure())))
        }
    }

    //
    // ----------------
    // Γ, x : T ⊢ x : T
    override fun Ast.Term.Hole<C>.run(i: Bindings<C>): Pair<Ast.Term<C>?, List<Proof<C>>> =
        i.getSignature(value)?.let { it to listOf() }
            ?: (null to listOf(Proof.Failure(CompilationException.Unbound(this))))
}