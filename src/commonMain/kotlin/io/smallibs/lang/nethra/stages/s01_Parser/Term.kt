package io.smallibs.lang.nethra.stages.s01_Parser

import io.smallibs.lang.nethra.cst.Cst
import io.smallibs.lang.nethra.cst.Cst.Localised
import io.smallibs.lang.nethra.stages.s01_Parser.Commons.ARROW
import io.smallibs.lang.nethra.stages.s01_Parser.Commons.CASE
import io.smallibs.lang.nethra.stages.s01_Parser.Commons.COLON
import io.smallibs.lang.nethra.stages.s01_Parser.Commons.DISJUNCTION
import io.smallibs.lang.nethra.stages.s01_Parser.Commons.DOT
import io.smallibs.lang.nethra.stages.s01_Parser.Commons.ID
import io.smallibs.lang.nethra.stages.s01_Parser.Commons.LACC
import io.smallibs.lang.nethra.stages.s01_Parser.Commons.LPAR
import io.smallibs.lang.nethra.stages.s01_Parser.Commons.PRODUCT
import io.smallibs.lang.nethra.stages.s01_Parser.Commons.RACC
import io.smallibs.lang.nethra.stages.s01_Parser.Commons.RPAR
import io.smallibs.lang.nethra.stages.s01_Parser.Commons.SKIP
import io.smallibs.lang.nethra.stages.s01_Parser.Commons.localise
import io.smallibs.parsec.parser.Core.`try`
import io.smallibs.parsec.parser.Core.lazy
import io.smallibs.parsec.parser.Core.returns
import io.smallibs.parsec.parser.Flow.optrep
import io.smallibs.parsec.parser.Flow.or
import io.smallibs.parsec.parser.Flow.then
import io.smallibs.parsec.parser.Flow.thenLeft
import io.smallibs.parsec.parser.Flow.thenRight
import io.smallibs.parsec.parser.Literal.char
import io.smallibs.parsec.parser.Literal.delimitedChar
import io.smallibs.parsec.parser.Literal.delimitedString
import io.smallibs.parsec.parser.Literal.integer
import io.smallibs.parsec.parser.Literal.string
import io.smallibs.parsec.parser.Monad.bind
import io.smallibs.parsec.parser.Monad.map
import io.smallibs.parsec.parser.Parser
import io.smallibs.parsec.parser.Region

object Term {

    private val KIND_TYPE: Parser<Char, Localised<Cst.Term>> get() = localise(`try`(string("Type")) map { Cst.Term.Type })
    private val INT_TYPE: Parser<Char, Localised<Cst.Term>> get() = localise(`try`(string("Int")) map { Cst.Term.IntTypeLiteral })
    private val CHAR_TYPE: Parser<Char, Localised<Cst.Term>> get() = localise(`try`(string("Char")) map { Cst.Term.CharTypeLiteral })
    private val STRING_TYPE: Parser<Char, Localised<Cst.Term>> get() = localise(`try`(string("String")) map { Cst.Term.StringTypeLiteral })
    private val VAR: Parser<Char, Localised<Cst.Term>> get() = localise(ID map { Cst.Term.Var(it) })
    private val HOLE: Parser<Char, Localised<Cst.Term>>
        get() = localise(char('?') thenRight ID map {
            Cst.Term.Var(it,
                true)
        })

    private val INT: Parser<Char, Localised<Cst.Term>> get() = localise(`try`(integer) map { Cst.Term.IntLiteral(it) })
    private val CHAR: Parser<Char, Localised<Cst.Term>>
        get() = localise(`try`(delimitedChar) map {
            Cst.Term.CharLiteral(it)
        })
    private val STRING: Parser<Char, Localised<Cst.Term>>
        get() = localise(`try`(delimitedString) map {
            Cst.Term.StringLiteral(it)
        })

    private fun ptype(
        left: Parser<Char, String>,
        right: Parser<Char, String>,
        operator: Parser<Char, Int>,
        builder: (Int, String, Localised<Cst.Term>, Localised<Cst.Term>) -> Cst.Term,
        `try`: (Parser<Char, String>) -> Parser<Char, String> = { it },
    ): Parser<Char, Localised<Cst.Term>> =
        localise(`try`(left thenRight ID thenLeft COLON) then lazy(Term::term) thenLeft right then operator then lazy(
            Term::term) map {
            builder(it.first.second, it.first.first.first, it.first.first.second, it.second)
        })

    private fun pterm(
        left: Parser<Char, String>,
        right: Parser<Char, String>,
        builder: (String, Localised<Cst.Term>) -> Cst.Term,
    ): Parser<Char, Localised<Cst.Term>> =
        localise(`try`(left thenRight ID thenLeft right thenLeft DOT) then lazy(Term::sterm) map {
            builder(it.first, it.second)
        })

    private fun forallImplicit(): Parser<Char, Localised<Cst.Term>> =
        ptype(LACC, RACC, ARROW map { 0 }, { _, n, l, r ->
            Cst.Term.Forall(n, l, r, true)
        })

    private fun forallOrExists(): Parser<Char, Localised<Cst.Term>> =
        ptype(LPAR, RPAR, ARROW map { 0 } or (PRODUCT map { 1 }), { o, n, l, r ->
            if (o == 0) {
                Cst.Term.Forall(n, l, r, false)
            } else {
                Cst.Term.Exists(n, l, r)
            }
        }, ::`try`)

    private fun lambda(): Parser<Char, Localised<Cst.Term>> =
        pterm(LACC, RACC) { n, t -> Cst.Term.Lambda(n, t, true) } or
                pterm(LPAR, RPAR) { n, t -> Cst.Term.Lambda(n, t, false) }

    private fun case(): Parser<Char, Localised<Cst.Term>> =
        localise(CASE thenRight lazy(Term::sterm) then lazy(Term::sterm) then lazy(Term::sterm) map {
            Cst.Term.Case(it.first.first,
                it.first.second,
                it.second)
        })

    private fun sterm(): Parser<Char, Localised<Cst.Term>> =
        case() or
                KIND_TYPE or INT_TYPE or CHAR_TYPE or STRING_TYPE or
                HOLE or VAR or
                (LPAR thenRight lazy(Term::term) thenLeft RPAR) or
                INT or CHAR or STRING


    private fun mayBeProduct(left: Localised<Cst.Term>): Parser<Char, Cst.Term> =
        (PRODUCT thenRight lazy(Term::sterm) map {
            Cst.Term.Exists(null, left, it)
        }) or returns(left.value)

    private fun mayBeDisjunction(left: Localised<Cst.Term>): Parser<Char, Cst.Term> =
        (DISJUNCTION thenRight lazy(Term::sterm) map {
            Cst.Term.Disjunction(left, it)
        }) or returns(left.value)

    private fun mayBeArrow(left: Localised<Cst.Term>): Parser<Char, Cst.Term> = (ARROW thenRight lazy(Term::term) map {
        Cst.Term.Forall(null, left, it, false)
    }) or returns(left.value)

    private fun param(): Parser<Char, Pair<Boolean, Localised<Cst.Term>>> =
        (sterm() map { false to it }) or (LACC thenRight lazy(Term::term) thenLeft RACC map { true to it })

    private fun apply(): Parser<Char, Localised<Cst.Term>> = localise(sterm() then lazy(Term::param).optrep map {
        if (it.second.isEmpty()) {
            it.first.value
        } else {
            it.second.fold(it.first) { acc, type ->
                Localised(Cst.Term.Apply(acc, type.second, type.first),
                    Region.T(acc.region.begin, type.second.region.end))
            }.value
        }
    })

    private fun term(): Parser<Char, Localised<Cst.Term>> =
        lambda() or forallOrExists() or forallImplicit() or
                localise(localise(localise(apply() bind Term::mayBeProduct) bind Term::mayBeDisjunction) bind Term::mayBeArrow)

    operator fun invoke(): Parser<Char, Localised<Cst.Term>> = SKIP thenRight term()
}