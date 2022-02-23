package io.smallibs.lang.nethra.stages.s03_Checker

import io.kotest.core.spec.style.StringSpec
import io.smallibs.lang.nethra.ast.Ast
import io.smallibs.lang.nethra.stages.s01_Parser.ParserStage
import io.smallibs.lang.nethra.stages.s02_Abstraction.AbstractionStage

class CheckerStageTest : StringSpec({

    "[checker] zero" {
        """
        sig zero : int
        def zero = 0  
        """.trimIndent().let { check(it) }
    }

    "[checker] combine/add" {
        """
        sig combine : {X:type} -> X -> X -> X
        
        sig add : int -> int -> int    
        def add = combine {int}
        """.trimIndent().let { check(it) }
    }

    "[checker] combineInt/add" {
        """
        sig add : int -> int -> int
        
        sig combineInt : (X).(X -> X -> X) int   
        def combineInt = add
        """.trimIndent().let { check(it) }
    }

    "[checker] combine/combineInt/add" {
        """
        sig combine : (x:type) -> type
        def combine = (X).(X -> X -> X)
            
        sig add : int -> int -> int
        
        sig combineInt : (X).(X -> X -> X) int   
        def combineInt = add
        """.trimIndent().let { check(it) }
    }
}) {

    companion object {
        fun check(program: String): List<Ast.Binding<Nothing>> =
            (ParserStage() compile program).let { bindings ->
                AbstractionStage() compile bindings
            }.let { bindings ->
                CheckerStage<Nothing>() compile bindings
            }
    }
}