package io.smallibs.lang.nethra.ast

interface Interpret<C, I, O> {
    fun Term.Type<C>.run(i: I): O
    fun Term.Data<C>.run(i: I): O
    fun Term.Id<C>.run(i: I): O

    fun Term.Lit<C>.run(i: I): O

    fun Term.Pi<C>.run(i: I): O
    fun Term.Lambda<C>.run(i: I): O
    fun Term.Apply<C>.run(i: I): O

    fun Term.Sigma<C>.run(i: I): O
    fun Term.Couple<C>.run(i: I): O
    fun Term.Fst<C>.run(i: I): O
    fun Term.Snd<C>.run(i: I): O

    fun Term.Disjunction<C>.run(i: I): O
    fun Term.Inl<C>.run(i: I): O
    fun Term.Inr<C>.run(i: I): O
    fun Term.Case<C>.run(i: I): O

    fun Term.Rec<C>.run(i: I): O
    fun Term.Fold<C>.run(i: I): O
    fun Term.Unfold<C>.run(i: I): O

    fun Term.Inhabit<C>.run(i: I): O

    fun Term.Hole<C>.run(i: I): O

    fun Term<C>.run(i: I): O =
        when (this) {
            is Term.Type -> run(i)
            is Term.Data -> run(i)
            is Term.Id -> run(i)

            is Term.Lit -> run(i)

            is Term.Pi -> run(i)
            is Term.Lambda -> run(i)
            is Term.Apply -> run(i)

            is Term.Sigma -> run(i)
            is Term.Couple -> run(i)
            is Term.Fst -> run(i)
            is Term.Snd -> run(i)

            is Term.Disjunction -> run(i)
            is Term.Inl -> run(i)
            is Term.Inr -> run(i)
            is Term.Case -> run(i)

            is Term.Rec -> run(i)
            is Term.Fold -> run(i)
            is Term.Unfold -> run(i)

            is Term.Inhabit -> run(i)

            is Term.Hole -> run(i)
        }
}