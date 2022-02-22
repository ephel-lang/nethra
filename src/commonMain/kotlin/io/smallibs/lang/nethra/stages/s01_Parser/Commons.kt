package io.smallibs.lang.nethra.stages.s01_Parser

import io.smallibs.lang.nethra.cst.Cst
import io.smallibs.parsec.parser.Core
import io.smallibs.parsec.parser.Flow.optrep
import io.smallibs.parsec.parser.Flow.or
import io.smallibs.parsec.parser.Flow.rep
import io.smallibs.parsec.parser.Flow.thenLeft
import io.smallibs.parsec.parser.Literal
import io.smallibs.parsec.parser.Monad.map
import io.smallibs.parsec.parser.Monad.satisfy
import io.smallibs.parsec.parser.Parser
import io.smallibs.parsec.parser.Region.locate

object Commons {

    val SKIP get() = Literal.charIn(" \t\n\r").optrep map {}

    fun <A> token(p: Parser<Char, A>) = p thenLeft SKIP
    fun localise(p: Parser<Char, Cst.Term>) = token(p.locate() map { Cst.Term.Localised(it.second, it.first) })

    val operators: List<String>
        get() = listOf("->", ".", "(", ")", "{", "}", ":", "*", "|", "=")

    val keywords: List<String>
        get() = listOf("sig", "def", "Type", "Int", "Char", "String", "case")

    val ID
        get() = token((Literal.charIn('A'..'Z') or Literal.charIn('a'..'z') or Literal.charIn("_")).rep) map
                { it.joinToString("") } satisfy { !keywords.contains(it) }

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
    val SIG get() = token(Literal.string("sig"))
    val DEF get() = token(Literal.string("def"))
}