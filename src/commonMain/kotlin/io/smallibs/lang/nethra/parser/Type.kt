package io.smallibs.lang.nethra.parser

import io.smallibs.lang.nethra.parser.Cst.Type.Localised
import io.smallibs.parsec.parser.Core.`try`
import io.smallibs.parsec.parser.Core.lazy
import io.smallibs.parsec.parser.Core.returns
import io.smallibs.parsec.parser.Flow.optrep
import io.smallibs.parsec.parser.Flow.or
import io.smallibs.parsec.parser.Flow.rep
import io.smallibs.parsec.parser.Flow.then
import io.smallibs.parsec.parser.Flow.thenLeft
import io.smallibs.parsec.parser.Flow.thenRight
import io.smallibs.parsec.parser.Literal.char
import io.smallibs.parsec.parser.Literal.charIn
import io.smallibs.parsec.parser.Literal.string
import io.smallibs.parsec.parser.Monad.bind
import io.smallibs.parsec.parser.Monad.map
import io.smallibs.parsec.parser.Parser
import io.smallibs.parsec.parser.Region
import io.smallibs.parsec.parser.Region.locate

object Type {

    private val SKIP get() = charIn(" \t\n\r").optrep map {}

    private fun <A> Token(p: Parser<Char, A>) = p thenLeft SKIP
    private fun Localise(p: Parser<Char, Cst.Type>) = Token(p.locate() map { Localised(it.second, it.first) })

    private val ID
        get() = Token((charIn('A'..'Z') or charIn('a'..'z') or charIn("_")).rep).map { it.joinToString("") }

    private val ARROW get() = Token(string("->"))
    private val LPAR get() = Token(char('(')) map { it.toString() }
    private val RPAR get() = Token(char(')')) map { it.toString() }
    private val LACC get() = Token(char('{')) map { it.toString() }
    private val RACC get() = Token(char('}')) map { it.toString() }
    private val SCOL get() = Token(char(':')) map { it.toString() }
    private val COMMA get() = Token(char(',')) map { it.toString() }

    private val TYPE get() = Localise(string("Type") map { Cst.Type.Kind })
    private val INT get() = Localise(string("Int") map { Cst.Type.IntLiteral })
    private val CHAR get() = Localise(string("Char") map { Cst.Type.CharLiteral })
    private val VAR get() = Localise(ID map { Cst.Type.Var(it) })

    private fun ptype(
        left: Parser<Char, String>,
        right: Parser<Char, String>,
        implicit: Boolean,
    ): Parser<Char, Localised> =
        Localise(`try`(left thenRight ID thenLeft SCOL) then lazy(::type) thenLeft right thenLeft ARROW then lazy(::type) map {
            Cst.Type.Fun(it.first.first, it.first.second, it.second, implicit)
        })

    private fun ptype(): Parser<Char, Localised> = ptype(LPAR, RPAR, false) or ptype(LACC, RACC, true)

    private fun stype(): Parser<Char, Localised> =
        TYPE or INT or CHAR or VAR or (LPAR thenRight lazy(::type) thenLeft RPAR)

    private fun mayBeArrowOrTuple(left: Localised): Parser<Char, Cst.Type> =
        (ARROW thenRight lazy(::type) map {
            Cst.Type.Fun(null, left, it, false)
        }) or (COMMA thenRight lazy(::type) map {
            Cst.Type.Tuple(left, it)
        }) or returns(left.type)

    private fun apply(): Parser<Char, Localised> =
        Localise(stype() then (LACC thenRight (lazy(::type) map { true to it }) thenLeft RACC or (stype() map { false to it })).optrep map {
            if (it.second.isEmpty()) it.first.type
            else {
                it.second.fold(it.first) { acc, type ->
                    Localised(Cst.Type.Apply(acc, type.second, type.first),
                        Region.T(acc.region.begin, type.second.region.end))
                }.type
            }
        })

    private fun type(): Parser<Char, Localised> = ptype() or Localise(apply() bind { mayBeArrowOrTuple(it) })

    operator fun invoke(): Parser<Char, Localised> = SKIP thenRight type()
}