package io.smallibs.lang.nethra.ast.impl

import io.smallibs.lang.nethra.ast.Ast
import io.smallibs.lang.nethra.ast.Variables
import io.smallibs.lang.nethra.ast.Visitor

class FreeVariableImpl<C> : Variables<C>, Visitor<C, Set<String>, Set<String>> {

    override fun free(t: Ast.Term<C>): Set<String> = setOf<String>().free(t)

    /**
     * Visitor implementation
     */

    override fun Ast.Term.Type<C>.run(i: Set<String>): Set<String> = setOf()

    override fun Ast.Term.Id<C>.run(i: Set<String>): Set<String> = if (i.contains(value)) {
        setOf()
    } else {
        setOf(value)
    }

    override fun Ast.Term.Lit<C>.run(i: Set<String>): Set<String> = setOf()

    override fun Ast.Term.Pi<C>.run(i: Set<String>): Set<String> = i.free(bound) + (i + this.n).free(body)

    override fun Ast.Term.Lambda<C>.run(i: Set<String>): Set<String> = (i + this.n).free(body)

    override fun Ast.Term.Apply<C>.run(i: Set<String>): Set<String> = i.free(abstraction) + i.free(argument)

    override fun Ast.Term.Sigma<C>.run(i: Set<String>): Set<String> = i.free(bound) + (i + this.n).free(body)

    override fun Ast.Term.Couple<C>.run(i: Set<String>): Set<String> = i.free(lhd) + i.free(rhd)

    override fun Ast.Term.Fst<C>.run(i: Set<String>): Set<String> = i.free(term)

    override fun Ast.Term.Snd<C>.run(i: Set<String>): Set<String> = i.free(term)

    override fun Ast.Term.Disjunction<C>.run(i: Set<String>): Set<String> = i.free(lhd) + i.free(rhd)

    override fun Ast.Term.Inl<C>.run(i: Set<String>): Set<String> = i.free(term)

    override fun Ast.Term.Inr<C>.run(i: Set<String>): Set<String> = i.free(term)

    override fun Ast.Term.Case<C>.run(i: Set<String>): Set<String> = i.free(term) + i.free(left) + i.free(right)

    override fun Ast.Term.Rec<C>.run(i: Set<String>): Set<String> = (i + self).free(body)

    override fun Ast.Term.Fold<C>.run(i: Set<String>): Set<String> = i.free(term)

    override fun Ast.Term.Unfold<C>.run(i: Set<String>): Set<String> = i.free(term)

    override fun Ast.Term.Hole<C>.run(i: Set<String>): Set<String> = ref.value?.let { i.free(it) } ?: setOf()

    //
    // Private behavior
    //

    private fun Set<String>.free(t: Ast.Term<C>): Set<String> {
        return t.run(this)
    }

}