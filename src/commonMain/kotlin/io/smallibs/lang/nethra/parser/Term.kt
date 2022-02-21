package io.smallibs.lang.nethra.parser

import io.smallibs.lang.nethra.parser.Cst.Term.Localised
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

object Term {

    private val SKIP get() = charIn(" \t\n\r").optrep map {}

    private fun <A> token(p: Parser<Char, A>) = p thenLeft SKIP
    private fun localise(p: Parser<Char, Cst.Term>) = token(p.locate() map { Localised(it.second, it.first) })

    private val ID
        get() = token((charIn('A'..'Z') or charIn('a'..'z') or charIn("_")).rep).map { it.joinToString("") }

    private val ARROW get() = token(`try`(string("->")))
    private val DOT get() = token(char('.')) map { it.toString() }
    private val LPAR get() = token(char('(')) map { it.toString() }
    private val RPAR get() = token(char(')')) map { it.toString() }
    private val LACC get() = token(char('{')) map { it.toString() }
    private val RACC get() = token(char('}')) map { it.toString() }
    private val COLON get() = token(char(':')) map { it.toString() }
    private val PRODUCT get() = token(char('*')) map { it.toString() }
    private val DISJUNCTION get() = token(char('|')) map { it.toString() }
    private val CASE get() = token(string("case"))

    private val Term get() = localise(`try`(string("Type")) map { Cst.Term.Kind })
    private val INT get() = localise(`try`(string("Int")) map { Cst.Term.IntLiteral })
    private val CHAR get() = localise(`try`(string("Char")) map { Cst.Term.CharLiteral })
    private val VAR get() = localise(ID map { Cst.Term.Var(it) })
    private val HOLE get() = localise(char('?') thenRight ID map { Cst.Term.Var(it, true) })

    private fun ptype(
        left: Parser<Char, String>,
        right: Parser<Char, String>,
        operator: Parser<Char, Int>,
        builder: (Int, String, Localised, Localised) -> Cst.Term,
    ): Parser<Char, Localised> =
        localise(`try`(left thenRight ID thenLeft COLON) then lazy(::term) thenLeft right then operator then lazy(::term) map {
            builder(it.first.second, it.first.first.first, it.first.first.second, it.second)
        })

    private fun pterm(
        left: Parser<Char, String>,
        right: Parser<Char, String>,
        builder: (String, Localised) -> Cst.Term,
    ): Parser<Char, Localised> =
        localise(`try`(left thenRight ID thenLeft right thenLeft DOT) then lazy(::sterm) map {
            builder(it.first, it.second)
        })

    private fun forallImplicit(): Parser<Char, Localised> =
        ptype(LACC, RACC, ARROW map { 0 }) { _, n, l, r ->
            Cst.Term.Forall(n, l, r, true)
        }

    private fun forallOrExists(): Parser<Char, Localised> =
        ptype(LPAR, RPAR, ARROW map { 0 } or (PRODUCT map { 1 })) { o, n, l, r ->
            if (o == 0) {
                Cst.Term.Forall(n, l, r, false)
            } else {
                Cst.Term.Exists(n, l, r)
            }
        }

    private fun lambda(): Parser<Char, Localised> =
        pterm(LACC, RACC) { n, t -> Cst.Term.Lambda(n, t, true) } or
                pterm(LPAR, RPAR) { n, t -> Cst.Term.Lambda(n, t, false) }

    private fun case(): Parser<Char, Localised> =
        localise(CASE thenRight lazy(::sterm) then lazy(::sterm) then lazy(::sterm) map {
            Cst.Term.Case(it.first.first,
                it.first.second,
                it.second)
        })

    private fun sterm(): Parser<Char, Localised> =
        Term or INT or CHAR or HOLE or VAR or (LPAR thenRight lazy(::term) thenLeft RPAR)


    private fun mayBeProduct(left: Localised): Parser<Char, Cst.Term> = (PRODUCT thenRight lazy(::sterm) map {
        Cst.Term.Exists(null, left, it)
    }) or returns(left.term)

    private fun mayBeDisjunction(left: Localised): Parser<Char, Cst.Term> = (DISJUNCTION thenRight lazy(::sterm) map {
        Cst.Term.Disjunction(left, it)
    }) or returns(left.term)

    private fun mayBeArrow(left: Localised): Parser<Char, Cst.Term> = (ARROW thenRight lazy(::term) map {
        Cst.Term.Forall(null, left, it, false)
    }) or returns(left.term)

    private fun param(): Parser<Char, Pair<Boolean, Localised>> =
        (sterm() map { false to it }) or (LACC thenRight lazy(::term) thenLeft RACC map { true to it })

    private fun apply(): Parser<Char, Localised> = localise(sterm() then lazy(::param).optrep map {
        if (it.second.isEmpty()) {
            it.first.term
        } else {
            it.second.fold(it.first) { acc, type ->
                Localised(Cst.Term.Apply(acc, type.second, type.first),
                    Region.T(acc.region.begin, type.second.region.end))
            }.term
        }
    })

    private fun term(): Parser<Char, Localised> =
        case() or
        forallOrExists() or
                forallImplicit() or
                lambda() or
                localise(localise(localise(apply() bind ::mayBeProduct) bind ::mayBeDisjunction) bind ::mayBeArrow)

    operator fun invoke(): Parser<Char, Localised> = SKIP thenRight term()
}