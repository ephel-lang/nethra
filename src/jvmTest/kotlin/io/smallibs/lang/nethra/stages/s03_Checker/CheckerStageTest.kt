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

    "[checker] trait/denotation" {
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

    "[checker] trait/extension/denotation"  {
        """
        sig A_T  : type -> type
        def A_T  = (X).((t:type) * (t * (t -> int) * X))
        
        sig Am_T : type
        def Am_T = (t:type) * t
            
        sig A_m : {X:type} -> A_T X -> Am_T
        def A_m = (x).(fst x, fst snd x)

        sig An_T : type
        def An_T = (t:type) * (t -> int)

        sig A_n : {X:type} -> A_T X -> An_T
        def A_n = (x).(fst x, fst snd snd x)
              
        sig unit  : type        
        sig value : A_T unit -> int
        def value = (x).((snd (A_n x)) (snd (A_m x)))        
        """.trimIndent().let { check(it) }
    }

    "[checker] list/deconstruction/isEmpty" {
        """
        sig unit : type
        sig Unit : unit
        
        sig list : type -> type
        
        sig nil  : {X:type} -> list X
        def nil  = fold (inl Unit)
        
        sig cons : {X:type} -> X -> list X -> list X
        def cons = (head).(tail).fold inr (head,tail)

        def list = (X).rec(l).(unit | (X * l)) 
        
        sig head : {X:type} -> X * list X -> X
        def head = (c).fst c

        sig tail : {X:type} -> X * list X -> list X
        def tail = (c).snd c
                
        sig bool : type
        def bool = true | false

        sig true : type
        sig True : true

        sig false : type
        sig False : false
        
        sig isEmpty : {X:type} -> list X -> bool
        def isEmpty = (l).case (unfold l) (_).(inl True) (_).(inr False)
        """.trimIndent().let { check(it) }
    }

    "[checker] refl/sym/trans [wip]" {
        """
        sig unit : type
        sig Unit : unit

        sig refl : {A:type} -> (x:A) -> (y:A) -> type
        
        sig Refl : {A:type} -> {x:A} -> refl {A} x x
               
        def refl = {A}.(x).(y).unit
         
        sig sym : {A:type} -> {x:A} -> {y:A}
            -> refl x y 
               --------
            -> refl y x 
            
        -- TODO def sym = (r).Refl
                       
        sig trans : {A:type} -> {x:A} -> {y:A} -> {z:A}
            -> refl x y
            -> refl y z
               --------
            -> refl x z            
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