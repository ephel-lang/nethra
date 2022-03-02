package io.smallibs.lang.nethra.stages.s01_Parser.internal

import io.smallibs.lang.nethra.cst.Cst
import io.smallibs.parsec.parser.Core
import io.smallibs.parsec.parser.Core.not
import io.smallibs.parsec.parser.Flow.opt
import io.smallibs.parsec.parser.Flow.optrep
import io.smallibs.parsec.parser.Flow.or
import io.smallibs.parsec.parser.Flow.then
import io.smallibs.parsec.parser.Flow.thenLeft
import io.smallibs.parsec.parser.Literal.char
import io.smallibs.parsec.parser.Literal.charIn
import io.smallibs.parsec.parser.Literal.string
import io.smallibs.parsec.parser.Monad.map
import io.smallibs.parsec.parser.Monad.satisfy
import io.smallibs.parsec.parser.Parser
import io.smallibs.parsec.parser.Region.locate

object Commons {

    private val LINE_COMMENT get() = string("--") then not(char('\n')).optrep then char('\n').opt
    private val BLOCK_COMMENT get() = string("-{") then not(string("}-")).optrep then string("}-")

    val SKIP get() = (BLOCK_COMMENT or LINE_COMMENT or charIn(" \t\n\r")).optrep map {}

    fun <A> token(p: Parser<Char, A>) = p thenLeft SKIP
    fun <T> localise(p: Parser<Char, T>) = token(p.locate() map { Cst.Localised(it.second, it.first) })

    val operators: List<String>
        get() = listOf("->", ".", "(", ")", "{", "}", ":", "*", "|", "=", "--", "—{", "}-")

    private val keywords: List<String>
        get() = listOf("sig",
            "def",
            "type",
            "case",
            "inl",
            "inr",
            "fst",
            "snd",
            "rec",
            "fold",
            "unfold",
            "data")

    val LETTER
        get() = charIn('A'..'Z') or charIn('a'..'z') or charIn("_")

    val NUMBER
        get() = charIn('0'..'9')

    val ID
        get() = token(LETTER then (LETTER or NUMBER).optrep) map
                { it.first + it.second.joinToString("") } satisfy { !keywords.contains(it) }

    val ARROW get() = token(Core.`try`(string("->")))
    val DOT get() = token(char('.')) map { it.toString() }
    val LPAR get() = token(char('(')) map { it.toString() }
    val RPAR get() = token(char(')')) map { it.toString() }
    val LACC get() = token(char('{')) map { it.toString() }
    val RACC get() = token(char('}')) map { it.toString() }
    val COLON get() = token(char(':')) map { it.toString() }
    val PRODUCT get() = token(char('*')) map { it.toString() }
    val COUPLE get() = token(char(',')) map { it.toString() }
    val DISJUNCTION get() = token(char('|')) map { it.toString() }
    val EQUAL get() = token(char('=')) map { it.toString() }

    val SIG get() = token(string("sig"))
    val DEF get() = token(string("def"))
    val CASE get() = token(string("case"))
    val INL get() = token(string("inl"))
    val INR get() = token(string("inr"))
    val FST get() = token(string("fst"))
    val SND get() = token(string("snd"))
    val FOLD get() = token(string("fold"))
    val UNFOLD get() = token(string("unfold"))
    val REC get() = token(string("rec"))
    val DATA get() = token(string("data"))
}