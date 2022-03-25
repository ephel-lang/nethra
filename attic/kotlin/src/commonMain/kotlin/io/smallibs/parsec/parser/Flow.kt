package io.smallibs.parsec.parser

import io.smallibs.parsec.io.Reader
import io.smallibs.parsec.parser.Core.returns
import io.smallibs.parsec.parser.Monad.bind
import io.smallibs.parsec.parser.Monad.map
import io.smallibs.parsec.parser.Response.Accept
import io.smallibs.parsec.parser.Response.Reject

object Flow {

    infix fun <I, A, B> Parser<I, A>.then(p: Parser<I, B>): Parser<I, Pair<A, B>> =
        this bind { a -> p map { a to it } }

    //
    // Alternate Then
    //

    fun <I, A, B> Parser<I, Pair<A, B>>.first(): Parser<I, A> =
        this map { it.first }

    infix fun <I, A, B> Parser<I, A>.thenLeft(p: Parser<I, B>): Parser<I, A> =
        (this then p).first()

    fun <I, A, B> Parser<I, Pair<A, B>>.second(): Parser<I, B> =
        this map { it.second }

    infix fun <I, A, B> Parser<I, A>.thenRight(p: Parser<I, B>): Parser<I, B> =
        (this then p).second()

    //
    // Choice
    //

    infix fun <I, A> Parser<I, A>.or(p: Parser<I, A>): Parser<I, A> = { reader ->
        val a = this(reader)
        when (a.consumed) {
            true -> a
            false -> a.fold({ a }, { p(reader) })
        }
    }

    //
    // Kleene operator, optional
    //

    val <I, A> Parser<I, A>.opt: Parser<I, A?>
        get() =
            this map { it } or returns<I, A?>(null)

    val <I, A> Parser<I, A>.optrep: Parser<I, List<A>>
        get() =
            { repeat(this, listOf(), false, it) }

    val <I, A> Parser<I, A>.rep: Parser<I, List<A>>
        get() =
            this then this.optrep map { listOf(it.first) + it.second }

    private tailrec fun <I, A> repeat(
        p: Parser<I, A>,
        acc: List<A>,
        consumed: Boolean,
        reader: Reader<I>,
    ): Response<I, List<A>> =
        when (val a = p(reader)) {
            is Reject -> Accept(acc, reader, consumed)
            is Accept -> repeat(p, acc + a.value, consumed || a.consumed, a.input)
        }

    //
    // End of stream
    //

    fun <A> eos(): Parser<A, Unit> = {
        when (it.read()) {
            null -> Accept(Unit, it, false)
            else -> Reject(it.location(), false)
        }
    }
}