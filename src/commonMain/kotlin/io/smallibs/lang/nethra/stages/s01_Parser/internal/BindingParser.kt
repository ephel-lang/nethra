package io.smallibs.lang.nethra.stages.s01_Parser.internal

import io.smallibs.lang.nethra.cst.Cst
import io.smallibs.lang.nethra.stages.s01_Parser.internal.Commons.COLON
import io.smallibs.lang.nethra.stages.s01_Parser.internal.Commons.DEF
import io.smallibs.lang.nethra.stages.s01_Parser.internal.Commons.EQUAL
import io.smallibs.lang.nethra.stages.s01_Parser.internal.Commons.ID
import io.smallibs.lang.nethra.stages.s01_Parser.internal.Commons.SIG
import io.smallibs.lang.nethra.stages.s01_Parser.internal.Commons.localise
import io.smallibs.parsec.parser.Flow.or
import io.smallibs.parsec.parser.Flow.then
import io.smallibs.parsec.parser.Flow.thenLeft
import io.smallibs.parsec.parser.Flow.thenRight
import io.smallibs.parsec.parser.Monad.map
import io.smallibs.parsec.parser.Parser

object BindingParser {

    private val sig: Parser<Char, Cst.Localised<Cst.Binding>> =
        localise(SIG thenRight ID thenLeft COLON then TermParser() map { Cst.Binding.Signature(it.first, it.second) })

    private val def: Parser<Char, Cst.Localised<Cst.Binding>> =
        localise(DEF thenRight ID thenLeft EQUAL then TermParser() map { Cst.Binding.Definition(it.first, it.second) })

    operator fun invoke(): Parser<Char, Cst.Localised<Cst.Binding>> = sig or def

}