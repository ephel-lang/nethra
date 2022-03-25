package io.smallibs.parsec.io

import io.smallibs.parsec.utils.Location

interface Reader<out A> {
    fun location(): Location
    fun read(): Pair<A, Reader<A>>?

    private class FromList(
        private val source: List<Char>,
        private val location: Location,
    ) : Reader<Char> {
        override fun location() =
            location

        override fun read() =
            source.getOrNull(location.position)?.let {
                it to FromList(source, location.step(it))
            }
    }

    companion object {
        fun string(s: String): Reader<Char> =
            FromList(s.toList(), Location())
    }
}
