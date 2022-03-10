package io.smallibs.lang.nethra.stages.report.impl

import io.smallibs.lang.nethra.ast.Printer
import io.smallibs.lang.nethra.stages.report.ProofPrinter
import io.smallibs.lang.nethra.stages.s03_Checker.internal.Proof

class ProofPrinterImpl<C>(val printer: Printer<C> = Printer()) : ProofPrinter<C>, Printer<C> by printer {
    override fun print(proof: Proof<C>, depth: Int, multiple: List<Int>) {
        if (depth >= 0) {
            print((0 until depth).map {
                if (multiple.contains(it)) {
                    "   |"
                } else {
                    "    "
                }
            }.joinToString("") + "   +")
        }
        when (proof) {
            is Proof.Failure -> println(" ❌")
            is Proof.Step -> {
                print(proof.conclusion)
                proof.premisses.forEachIndexed { index, it ->
                    print(it,
                        depth + 1,
                        if (proof.premisses.size > 1 && index < proof.premisses.size - 1) multiple + (depth + 1) else multiple)
                }
            }
        }
    }

    private fun print(goal: Proof.Goal<C>) {
        when (goal) {
            is Proof.Check -> println(" Γ ⊢ ${goal.term.prettyPrint()} : ${goal.type.prettyPrint()}")
            is Proof.Congruent -> println(" Γ ⊢ ${goal.expected.prettyPrint()} ≅ ${goal.computed.prettyPrint()}")
            is Proof.Infer -> println(" Γ ⊢ ${goal.term.prettyPrint()} : ${goal.type?.prettyPrint() ?: "?"}")
        }
    }
}