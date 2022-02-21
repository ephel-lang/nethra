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
    private val COLON get() = Token(char(':')) map { it.toString() }
    private val PRODUCT get() = Token(char('*')) map { it.toString() }

    private val TYPE get() = Localise(string("Type") map { Cst.Type.Kind })
    private val INT get() = Localise(string("Int") map { Cst.Type.IntLiteral })
    private val CHAR get() = Localise(string("Char") map { Cst.Type.CharLiteral })
    private val VAR get() = Localise(ID map { Cst.Type.Var(it) })
    private val HOLE get() = Localise(char('?') thenRight ID map { Cst.Type.Var(it, true) })

    private fun ptype(
        left: Parser<Char, String>,
        right: Parser<Char, String>,
        operator: Parser<Char, String>,
        builder: (String, String, Localised, Localised) -> Cst.Type,
        `try`: (Parser<Char, String>) -> Parser<Char, String> = { it },
    ): Parser<Char, Localised> =
        Localise(`try`(left thenRight ID thenLeft COLON) then lazy(::type) thenLeft right then operator then lazy(::type) map {
            builder(it.first.second, it.first.first.first, it.first.first.second, it.second)
        })

    private fun forallImplicit(): Parser<Char, Localised> =
        ptype(LACC, RACC, ARROW, { _, n, l, r -> Cst.Type.Forall(n, l, r, true) })

    private fun forallOrExists(): Parser<Char, Localised> =
        ptype(LPAR, RPAR, ARROW or PRODUCT, { o, n, l, r ->
            if (o == "->") {
                Cst.Type.Forall(n, l, r, false)
            } else {
                Cst.Type.Exists(n, l, r)
            }
        }, ::`try`)

    private fun stype(): Parser<Char, Localised> =
        TYPE or INT or CHAR or HOLE or VAR or (LPAR thenRight lazy(::type) thenLeft RPAR)

    private fun mayBeProduct(left: Localised): Parser<Char, Cst.Type> =
        (PRODUCT thenRight lazy(::stype) map {
            Cst.Type.Exists(null, left, it)
        }) or returns(left.type)

    private fun mayBeArrow(left: Localised): Parser<Char, Cst.Type> =
        (ARROW thenRight lazy(::type) map {
            Cst.Type.Forall(null, left, it, false)
        }) or returns(left.type)

    private fun param(): Parser<Char, Pair<Boolean, Localised>> =
        (stype() map { false to it }) or (LACC thenRight lazy(::type) thenLeft RACC map { true to it })

    private fun apply(): Parser<Char, Localised> =
        Localise(stype() then lazy(::param).optrep map {
            if (it.second.isEmpty()) it.first.type
            else {
                it.second.fold(it.first) { acc, type ->
                    Localised(Cst.Type.Apply(acc, type.second, type.first),
                        Region.T(acc.region.begin, type.second.region.end))
                }.type
            }
        })

    private fun type(): Parser<Char, Localised> =
        forallOrExists() or forallImplicit() or Localise(Localise(apply() bind { mayBeProduct(it) }) bind { mayBeArrow(it) })

    operator fun invoke(): Parser<Char, Localised> = SKIP thenRight type()
}