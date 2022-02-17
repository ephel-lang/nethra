package io.smallibs.parsec.parser

import io.smallibs.parsec.extension.pipe
import io.smallibs.parsec.parser.Core.fails
import io.smallibs.parsec.parser.Core.returns
import io.smallibs.parsec.parser.Response.Accept
import io.smallibs.parsec.parser.Response.Reject

object Monad {
    infix fun <I, A, B> Parser<I, A>.map(f: (A) -> B): Parser<I, B> = { reader ->
        this(reader).fold({
            Accept(f(it.value), it.input, it.consumed)
        }, {
            Reject(it.location, it.consumed)
        })
    }

    fun <I, A> join(p: Parser<I, Parser<I, A>>): Parser<I, A> = { reader ->
        when (val a = p(reader)) {
            is Accept -> {
                val b = a.value.invoke(a.input)
                when (b) {
                    is Reject -> Reject(b.location, b.consumed || a.consumed)
                    is Accept -> Accept(b.value, b.input, b.consumed || a.consumed)
                }
            }
            is Reject -> Reject(a.location, a.consumed)
        }
    }

    infix fun <I, A, B> Parser<I, A>.bind(f: (A) -> Parser<I, B>): Parser<I, B> =
        join(this map f)

    infix fun <I, A> Parser<I, A>.satisfy(p: (A) -> Boolean): Parser<I, A> =
        this bind {
            if (p(it)) {
                returns(it)
            } else {
                fails()
            }
        }

    infix fun <I, A, B> Parser<I, A>.applicative(p: Parser<I, (A) -> B>): Parser<I, B> =
        this bind { v -> p map { f -> f(v) } }

    infix fun <I, A, B, C> ((A) -> Parser<I, B>).then(p2: (B) -> Parser<I, C>): (A) -> Parser<I, C> =
        this pipe { it bind p2 }
}
