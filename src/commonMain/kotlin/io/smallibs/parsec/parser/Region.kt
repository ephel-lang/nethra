package io.smallibs.parsec.parser

import io.smallibs.parsec.utils.Location

object Region {

    val none = T(Location(), Location())

    data class T(val begin: Location, val end: Location)

    fun <I, O> Parser<I, O>.locate(): Parser<I, Pair<T, O>> = { reader ->
        this(reader).fold({
            Response.Accept(T(reader.location(), it.input.location()) to it.value, it.input, it.consumed)
        }, {
            Response.Reject(it.location, it.consumed)
        })
    }

}