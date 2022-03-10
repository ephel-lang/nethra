package io.smallibs.lang.nethra.stages.report

import io.smallibs.lang.nethra.stages.report.impl.ProofPrinterImpl
import io.smallibs.lang.nethra.stages.s03_Checker.internal.Proof

interface ProofPrinter<C> {

    fun print(proof: Proof<C>, depth: Int = -1, multiple: List<Int> = listOf())

    companion object {
        operator fun <C> invoke(): ProofPrinter<C> = ProofPrinterImpl()
    }
}