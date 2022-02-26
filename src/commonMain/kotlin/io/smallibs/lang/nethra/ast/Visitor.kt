package io.smallibs.lang.nethra.ast

interface Visitor<C, I, O> {
    fun Ast.Term.Type<C>.run(i: I): O
    fun Ast.Term.Id<C>.run(i: I): O

    fun Ast.Term.Lit<C>.run(i: I): O

    fun Ast.Term.Pi<C>.run(i: I): O
    fun Ast.Term.Lambda<C>.run(i: I): O
    fun Ast.Term.Apply<C>.run(i: I): O

    fun Ast.Term.Sigma<C>.run(i: I): O
    fun Ast.Term.Couple<C>.run(i: I): O
    fun Ast.Term.Fst<C>.run(i: I): O
    fun Ast.Term.Snd<C>.run(i: I): O

    fun Ast.Term.Disjunction<C>.run(i: I): O
    fun Ast.Term.Inl<C>.run(i: I): O
    fun Ast.Term.Inr<C>.run(i: I): O
    fun Ast.Term.Case<C>.run(i: I): O

    fun Ast.Term.Rec<C>.run(i: I): O
    fun Ast.Term.Fold<C>.run(i: I): O
    fun Ast.Term.Unfold<C>.run(i: I): O

    fun Ast.Term.Inhabit<C>.run(i: I): O

    fun Ast.Term.Hole<C>.run(i: I): O

    fun Ast.Term<C>.run(i: I): O =
        when (this) {
            is Ast.Term.Type -> run(i)
            is Ast.Term.Id -> run(i)

            is Ast.Term.Lit -> run(i)

            is Ast.Term.Pi -> run(i)
            is Ast.Term.Lambda -> run(i)
            is Ast.Term.Apply -> run(i)

            is Ast.Term.Sigma -> run(i)
            is Ast.Term.Couple -> run(i)
            is Ast.Term.Fst -> run(i)
            is Ast.Term.Snd -> run(i)

            is Ast.Term.Disjunction -> run(i)
            is Ast.Term.Inl -> run(i)
            is Ast.Term.Inr -> run(i)
            is Ast.Term.Case -> run(i)

            is Ast.Term.Rec -> run(i)
            is Ast.Term.Fold -> run(i)
            is Ast.Term.Unfold -> run(i)

            is Ast.Term.Inhabit -> run(i)

            is Ast.Term.Hole -> run(i)
        }
}