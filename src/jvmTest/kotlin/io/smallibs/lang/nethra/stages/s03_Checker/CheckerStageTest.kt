package io.smallibs.lang.nethra.stages.s03_Checker

import io.kotest.core.spec.style.StringSpec
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
        sig combine : {t:type} -> t -> t -> t
        
        sig add : int -> int -> int    
        def add = combine {int}
        """.trimIndent().let { check(it) }
    }

    "[checker] combineInt/add" {
        """
        sig add : int -> int -> int
        
        sig combineInt : (t).(t -> t -> t) int   
        def combineInt = add
        """.trimIndent().let { check(it) }
    }

    "[checker] combine/combineInt/add" {
        """
        sig combine : (x:type) -> type
        def combine = (t).(t -> t -> t)
            
        sig add : int -> int -> int
        
        sig combineInt : combine int   
        def combineInt = add
        """.trimIndent().let { check(it) }
    }

    "[checker] inl/inr/case" {
        """
        sig ic : int | char -> type
        def ic = (x).(case x (_).char (_).int)
        
        sig m1 : ic (inl 1)
        def m1 = 'c'

        sig m2 : ic (inr m1)
        def m2 = 1
        """.trimIndent().let { check(it) }
    }

    "[checker] inl/inr/case/2" {
        """
        sig ic : int | char -> type
        def ic = (x).(case x (_).char (_).int)
        
        sig m1 : ic (inl 1)
        def m1 = 'c'

        sig m2 : ic (inr 'c') | ic (inl 1) 
        def m2 = inl 1
        """.trimIndent().let { check(it) }
    }

    "[checker] exist" {
        """
        sig A : (t:type) * t
        def A = (char, 'c')
        """.trimIndent().let { check(it) }
    }

    "[checker] exist/fst/snd" {
        """
        sig A_T  : type
        def A_T  = (t:type) * (t * (t -> int))
        
        sig Am_T : type
        def Am_T = (t:type) * t
            
        sig An_T : type
        def An_T = (t:type) * (t -> int)

        sig A_m : A_T -> Am_T
        def A_m = (x).(fst x, fst snd x)

        sig A_n : A_T -> An_T
        def A_n = (x).(fst x, snd snd x)
        """.trimIndent().let { check(it) }
    }

}) {
    companion object {
        fun check(program: String) =
            (ParserStage() compile program).let { bindings ->
                AbstractionStage<Nothing>() compile bindings
            }.let { bindings ->
                CheckerStage<Nothing>() compile bindings
            }
    }
}