package io.smallibs.lang.nethra.stages.s01_Parser.internal

import io.smallibs.lang.nethra.cst.Cst
import io.smallibs.parsec.parser.Core
import io.smallibs.parsec.parser.Flow.optrep
import io.smallibs.parsec.parser.Flow.or
import io.smallibs.parsec.parser.Flow.rep
import io.smallibs.parsec.parser.Flow.then
import io.smallibs.parsec.parser.Flow.thenLeft
import io.smallibs.parsec.parser.Literal
import io.smallibs.parsec.parser.Monad.map
import io.smallibs.parsec.parser.Monad.satisfy
import io.smallibs.parsec.parser.Parser
import io.smallibs.parsec.parser.Region.locate

object Commons {

    val SKIP get() = Literal.charIn(" \t\n\r").optrep map {}

    fun <A> token(p: Parser<Char, A>) = p thenLeft SKIP
    fun <T> localise(p: Parser<Char, T>) = token(p.locate() map { Cst.Localised<T>(it.second, it.first) })

    val operators: List<String>
        get() = listOf("->", ".", "(", ")", "{", "}", ":", "*", "|", "=")

    private val keywords: List<String>
        get() = listOf("sig", "def", "type", "int", "char", "string", "case", "inl", "inr")

    val LETTER
        get() = Literal.charIn('A'..'Z') or Literal.charIn('a'..'z') or Literal.charIn("_")

    val NUMBER
        get() = Literal.charIn('0'..'9')

    val ID
        get() = token(LETTER then (LETTER or NUMBER).optrep) map
                { it.first + it.second.joinToString("") } satisfy { !keywords.contains(it) }

    val ARROW get() = token(Core.`try`(Literal.string("->")))
    val DOT get() = token(Literal.char('.')) map { it.toString() }
    val LPAR get() = token(Literal.char('(')) map { it.toString() }
    val RPAR get() = token(Literal.char(')')) map { it.toString() }
    val LACC get() = token(Literal.char('{')) map { it.toString() }
    val RACC get() = token(Literal.char('}')) map { it.toString() }
    val COLON get() = token(Literal.char(':')) map { it.toString() }
    val PRODUCT get() = token(Literal.char('*')) map { it.toString() }
    val DISJUNCTION get() = token(Literal.char('|')) map { it.toString() }
    val EQUAL get() = token(Literal.char('=')) map { it.toString() }
    val CASE get() = token(Literal.string("case"))
    val INL get() = token(Literal.string("inl"))
    val INR get() = token(Literal.string("inr"))
    val SIG get() = token(Literal.string("sig"))
    val DEF get() = token(Literal.string("def"))
}