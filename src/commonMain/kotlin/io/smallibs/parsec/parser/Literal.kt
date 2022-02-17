package io.smallibs.parsec.parser

import io.smallibs.parsec.extension.charsToFloat
import io.smallibs.parsec.extension.charsToInt
import io.smallibs.parsec.extension.stringsToString
import io.smallibs.parsec.parser.Core.`try`
import io.smallibs.parsec.parser.Core.any
import io.smallibs.parsec.parser.Core.not
import io.smallibs.parsec.parser.Core.returns
import io.smallibs.parsec.parser.Flow.opt
import io.smallibs.parsec.parser.Flow.optrep
import io.smallibs.parsec.parser.Flow.or
import io.smallibs.parsec.parser.Flow.rep
import io.smallibs.parsec.parser.Flow.then
import io.smallibs.parsec.parser.Flow.thenLeft
import io.smallibs.parsec.parser.Flow.thenRight
import io.smallibs.parsec.parser.Monad.map
import io.smallibs.parsec.parser.Monad.satisfy

//
// Number parser
//

object Literal {
    fun charIn(c: CharRange): Parser<Char, Char> =
        `try`(any<Char>() satisfy { c.contains(it) })

    fun charIn(s: String): Parser<Char, Char> =
        `try`(any<Char>() satisfy { it in s })

    fun charIn(vararg s: Char): Parser<Char, Char> =
        `try`(any<Char>() satisfy { it in s })

    fun char(c: Char): Parser<Char, Char> =
        charIn(c)

    val integer: Parser<Char, Int>
        get() =
            STRING_INTEGER map { it.charsToInt() }

    val float: Parser<Char, Float>
        get() =
            STRING_INTEGER then ((char('.') then STRING_NUMBER map {
                (listOf(it.first) + it.second)
            }).opt map {
                it ?: listOf()
            }) map {
                (it.first + it.second).charsToFloat()
            }

    fun string(s: String): Parser<Char, String> =
        s.fold(returns<Char, Unit>(Unit)) { a, c -> a thenLeft char(c) } map { s }

    fun delimitedString(): Parser<Char, String> =
        char('"') thenRight
                (`try`(string("\\\"")) or (not(char('"')).map(Char::toString))).optrep thenLeft
                char('"') map { it.stringsToString() }

    private val STRING_NUMBER: Parser<Char, List<Char>> =
        charIn('0'..'9').rep

    private val STRING_INTEGER: Parser<Char, List<Char>> =
        charIn("-+").opt map {
            it ?: '+'
        } then STRING_NUMBER map {
            listOf(it.first) + it.second
        }
}
