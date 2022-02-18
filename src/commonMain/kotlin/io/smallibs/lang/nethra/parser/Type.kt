package io.smallibs.lang.nethra.parser

import io.smallibs.lang.nethra.parser.Cst.Type.Localised
import io.smallibs.parsec.parser.Core.`try`
import io.smallibs.parsec.parser.Core.lazy
import io.smallibs.parsec.parser.Flow.opt
import io.smallibs.parsec.parser.Flow.optrep
import io.smallibs.parsec.parser.Flow.or
import io.smallibs.parsec.parser.Flow.rep
import io.smallibs.parsec.parser.Flow.then
import io.smallibs.parsec.parser.Flow.thenLeft
import io.smallibs.parsec.parser.Flow.thenRight
import io.smallibs.parsec.parser.Literal.char
import io.smallibs.parsec.parser.Literal.charIn
import io.smallibs.parsec.parser.Literal.string
import io.smallibs.parsec.parser.Monad.map
import io.smallibs.parsec.parser.Parser
import io.smallibs.parsec.parser.Region.locate

object Type {

    private val SKIP get() = charIn(" \t\n\r").optrep map {}

    private fun <A> Token(p: Parser<Char, A>) = p thenLeft SKIP
    private fun Localised(p: Parser<Char, Cst.Type>) = Token(p.locate() map { Localised(it.second, it.first) })

    private val ID
        get() = Token((charIn('A'..'Z') or charIn('a'..'z') or charIn("_")).rep).map { it.joinToString("") }

    private val ARROW get() = Token(string("->")) map {}
    private val LPAR get() = Token(char('(')) map {}
    private val RPAR get() = Token(char(')')) map {}
    private val LACC get() = Token(char('{')) map {}
    private val RACC get() = Token(char('}')) map {}
    private val SCOL get() = Token(char(':')) map {}

    private val TYPE get() = Localised(string("Type") map { Cst.Type.Kind })
    private val INT get() = Localised(string("Int") map { Cst.Type.IntLiteral })
    private val CHAR get() = Localised(string("Char") map { Cst.Type.CharLiteral })
    private val VAR get() = Localised(ID map { Cst.Type.Var(it) })

    private fun ptype(
        left: Parser<Char, Unit>,
        right: Parser<Char, Unit>,
        implicit: Boolean,
    ): Parser<Char, (Localised) -> Cst.Type.Fun> =
        (left thenRight ID thenLeft SCOL then lazy(::type) thenLeft right) map {
            { body: Localised ->
                Cst.Type.Fun(it.first, it.second, body, implicit)
            }
        }

    private fun ptype(): Parser<Char, (Localised) -> Cst.Type.Fun> =
        ptype(LPAR, RPAR, false) or ptype(LACC, RACC, true)

    private fun stype(): Parser<Char, Localised> =
        TYPE or INT or CHAR or VAR or (LPAR thenRight lazy(::type) thenLeft RPAR)

    private fun type(): Parser<Char, Localised> =
        Localised(
            (`try`(ptype()) thenLeft ARROW then lazy(::type) map {
                it.first(it.second)
            }) or (lazy(::stype) then (ARROW thenRight lazy(::type)).opt map {
                when (val type = it.second) {
                    null -> it.first.type
                    else -> Cst.Type.Fun(null, it.first, type, false)
                }
            })
        )

    operator fun invoke(): Parser<Char, Localised> =
        SKIP thenRight type()
}