package io.smallibs.parsec.utils

data class Location(val position: Int = 0, val line: Int = 1, val column: Int = 0) {
    fun step(c: Char): Location =
        when (c) {
            '\n' -> Location(position + 1, line + 1, 0)
            else -> Location(position + 1, line, column + 1)
        }
}

