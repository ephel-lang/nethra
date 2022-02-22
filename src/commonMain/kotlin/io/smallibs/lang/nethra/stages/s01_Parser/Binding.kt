package io.smallibs.lang.nethra.stages.s01_Parser

import io.smallibs.lang.nethra.cst.Cst
import io.smallibs.lang.nethra.stages.s01_Parser.Commons.COLON
import io.smallibs.lang.nethra.stages.s01_Parser.Commons.DEF
import io.smallibs.lang.nethra.stages.s01_Parser.Commons.EQUAL
import io.smallibs.lang.nethra.stages.s01_Parser.Commons.ID
import io.smallibs.lang.nethra.stages.s01_Parser.Commons.SIG
import io.smallibs.lang.nethra.stages.s01_Parser.Commons.localise
import io.smallibs.parsec.parser.Flow.or
import io.smallibs.parsec.parser.Flow.then
import io.smallibs.parsec.parser.Flow.thenLeft
import io.smallibs.parsec.parser.Flow.thenRight
import io.smallibs.parsec.parser.Monad.map
import io.smallibs.parsec.parser.Parser

object Binding {

    private val sig: Parser<Char, Cst.Localised<Cst.Binding>> =
        localise(SIG thenRight ID thenLeft COLON then Term() map { Cst.Binding.Signature(it.first, it.second) })

    private val def: Parser<Char, Cst.Localised<Cst.Binding>> =
        localise(DEF thenRight ID thenLeft EQUAL then Term() map { Cst.Binding.Definition(it.first, it.second) })

    operator fun invoke(): Parser<Char, Cst.Localised<Cst.Binding>> = sig or def

}