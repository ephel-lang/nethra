package io.smallibs.parsec.parser.json

import io.smallibs.parsec.parser.json.Json.JsonArray
import io.smallibs.parsec.parser.json.Json.JsonBoolean
import io.smallibs.parsec.parser.json.Json.JsonNull
import io.smallibs.parsec.parser.json.Json.JsonNumber
import io.smallibs.parsec.parser.json.Json.JsonObject
import io.smallibs.parsec.parser.json.Json.JsonString
import io.smallibs.parsec.io.Reader
import io.smallibs.parsec.io.skip
import io.smallibs.parsec.parser.Core.any
import io.smallibs.parsec.parser.Core.lazy
import io.smallibs.parsec.parser.Core.lookahead
import io.smallibs.parsec.parser.Flow.opt
import io.smallibs.parsec.parser.Flow.optrep
import io.smallibs.parsec.parser.Flow.rep
import io.smallibs.parsec.parser.Flow.then
import io.smallibs.parsec.parser.Flow.thenLeft
import io.smallibs.parsec.parser.Flow.thenRight
import io.smallibs.parsec.parser.Literal.char
import io.smallibs.parsec.parser.Literal.charIn
import io.smallibs.parsec.parser.Literal.delimitedString
import io.smallibs.parsec.parser.Literal.float
import io.smallibs.parsec.parser.Literal.string
import io.smallibs.parsec.parser.Monad.bind
import io.smallibs.parsec.parser.Monad.map
import io.smallibs.parsec.parser.Parser

object JsonParser {

    private val JSON_NULL: Parser<Char, Json> =
        string("null") map { JsonNull }

    private val JSON_TRUE: Parser<Char, Json> =
        string("true") map { JsonBoolean(true) }

    private val JSON_FALSE: Parser<Char, Json> =
        string("false") map { JsonBoolean(false) }

    private val JSON_INT: Parser<Char, Json> =
        float map { JsonNumber(it) }

    private val JSON_STRING: Parser<Char, Json> =
        delimitedString() map { JsonString(it) }

    private fun <A> structure(p: Parser<Char, A>, o: Char, s: Char, c: Char): Parser<Char, List<A>?> =
        char(o) thenRight (p then (char(s) thenRight p).optrep thenLeft char(c) map { (s, l) -> listOf(s) + l }).opt

    private val JSON_ARRAY: Parser<Char, Json> =
        structure(lazy { JSON }, '[', ',', ']') map { JsonArray(it.orEmpty()) }

    private val JSON_ATTRIBUTE: Parser<Char, Pair<String, Json>> =
        delimitedString() thenLeft char(':') then lazy { JSON }

    private val JSON_OBJECT: Parser<Char, Json> =
        structure(JSON_ATTRIBUTE, '{', ',', '}') map { JsonObject(it.orEmpty().toMap()) }

    val JSON: Parser<Char, Json> =
        lookahead(any<Char>()) bind {
            when (it) {
                'n' -> JSON_NULL
                't' -> JSON_TRUE
                'f' -> JSON_FALSE
                '"' -> JSON_STRING
                '[' -> JSON_ARRAY
                '{' -> JSON_OBJECT
                else -> JSON_INT
            }
        }

    fun reader(r: Reader<Char>): Reader<Char> = r skip charIn("\r\n\t ").rep

}
