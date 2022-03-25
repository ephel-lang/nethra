package io.smallibs.parsec.parser

import io.smallibs.parsec.parser.Response.Accept
import io.smallibs.parsec.parser.Response.Reject

object Core {
    fun <I, A> returns(v: A): Parser<I, A> = {
        Accept(v, it, false)
    }

    fun <I, A> fails(reason: String? = null): Parser<I, A> = {
        Reject(it.location(), false, reason)
    }

    fun <I> any(): Parser<I, I> = {
        it.read()?.let { Accept(it.first, it.second, true) } ?: Reject(it.location(), false)
    }

    fun <I> not(p: Parser<I, *>): Parser<I, I> = { reader ->
        p(reader).fold({ Reject(reader.location(), false) }, { any<I>()(reader) })
    }

    fun <I, A> lazy(f: () -> Parser<I, A>): Parser<I, A> = { reader -> f()(reader) }

    fun <I, A> `try`(p: Parser<I, A>): Parser<I, A> = { p(it).fold({ it }, { Reject(it.location, false) }) }

    fun <I, A> lookahead(p: Parser<I, A>): Parser<I, A> = { reader ->
        p(reader).fold({ Accept(it.value, reader, false) }, { Reject(it.location, false) })
    }
}