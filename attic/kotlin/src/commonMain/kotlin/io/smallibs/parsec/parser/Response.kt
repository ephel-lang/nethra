package io.smallibs.parsec.parser

import io.smallibs.parsec.io.Reader
import io.smallibs.parsec.utils.Location

sealed class Response<I, out A>(open val consumed: Boolean) {
    data class Accept<I, out A>(
        val value: A,
        val input: Reader<I>,
        override val consumed: Boolean,
    ) : Response<I, A>(consumed)

    data class Reject<I, out A>(
        val location: Location,
        override val consumed: Boolean,
        val reason: String? = null,
    ) : Response<I, A>(consumed)

    fun <B> fold(accept: (Accept<I, A>) -> B, reject: (Reject<I, A>) -> B): B =
        when (this) {
            is Accept -> accept(this)
            is Reject -> reject(this)
        }
}



