package io.smallibs.lang.nethra.stages.s03_Checker

import io.kotest.core.spec.style.StringSpec
import io.smallibs.lang.nethra.stages.errors.ErrorReporter
import io.smallibs.lang.nethra.stages.errors.ErrorReporter.Companion.invoke
import io.smallibs.lang.nethra.stages.s01_Parser.ParserStage
import io.smallibs.lang.nethra.stages.s02_Abstraction.AbstractionStage
import io.smallibs.parsec.parser.Region

class CheckerStageTest : StringSpec({

    "[checker] zero" {
        """
        sig zero : int
        def zero = 1
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
        def A = (char, '1')
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

    "[checker] list/sample" {
        """
        sig unit : type
        sig Unit  : unit

        sig list : type -> type
        def list = (X).rec(l).(unit | X * l)
        sig nil  : {X:type} -> list X
        sig cons : {X:type} -> X -> list X -> list X
        
        sig test : list int
        def test = cons 1 nil
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
        -{
        In Agda the reflexivity is expressed thanks to the GADT:
        ```agda
        data _≡_ {A : Set} (x : A) : A → Set where
            refl : x ≡ x
        ```            
        }-

        sig refl : {A:type} -> (x:A) -> (y:A) -> type
        sig Refl : {A:type} -> {x:A} -> refl {A} x x
         
        sig sym : {A:type} -> {x:A} -> {y:A}
                -> refl x y 
                   --------
                -> refl y x 
            
        -- def sym = (r).Refl                   
        """.trimIndent().let { check(it) }
    }

    "[checker] nat" {
        """
        sig unit : type
        sig Unit : unit
        
        -- Natural definition
        sig zeroT : type
        sig Zero  : zeroT
        
        sig succT : type
        sig Succ  : succT
        
        sig nat  : type
        def nat  = rec(l).(zeroT | succT * l)       

        sig nat2 : type
        def nat2 = nat -> nat

        sig zero : nat
        def zero = fold (inl Zero)
        sig succ : nat2
        def succ = (pred).fold inr (Succ, pred)
                
        sig incr : nat2
        def incr = succ
        
        sig add  : nat -> nat2
        def add  = (p1).(p2).(case (unfold p1) (_).p2 (s).(succ (add (snd s) p2)))
         
        sig two  : nat
        def two  = add (succ zero) (succ zero)
        """.trimIndent().let { check(it) }
    }

    "[checker] vector [wip]" {
        """
        sig unit : type
        sig Unit : unit
        
        -- Natural definition
        
        sig zeroT : type
        sig Zero  : zeroT
        
        sig succT : type
        sig Succ  : succT
        
        sig nat  : type
        def nat  = rec(l).(zeroT | succT * l)       

        sig nat2 : type
        def nat2 = nat -> nat

        sig zero : nat
        def zero = fold (inl Zero)
        sig succ : nat2
        def succ = (pred).fold inr (Succ, pred)

        sig add  : nat -> nat -> nat
        def add  = (p1).(p2).case unfold p1 (_).p2 (s).(succ (add snd s p2))

        -- Vector definition
        
        sig nilT : type
        sig Nil  : nilT
        
        sig consT : type
        sig Cons  : consT

        sig vect : type -> nat -> type
        def vect = (X).(s).rec(l).(nilT | consT * X * l)

        sig nil  : {X:type} -> vect X zero
        def nil  = let e = inl Nil in (fold e)        

        sig cons : {X:type} -> {n:nat} -> X -> vect X n -> vect X (succ n)
        def cons = (head).(tail).fold inr (Cons,head,tail)
        
        sig comb : {X:type} -> {n:nat} -> {m:nat} -> vect X n -> vect X m -> vect X (add n m)
        def comb = (v1).(v2).case unfold v1 (_).v2 (v1).(cons fst snd v1 (comb snd snd v1 v2))
        """.trimIndent().let { check(it) }
    }

    "[checker] term/inhabits/type" {
        """
            sig isA : (T:type) -> T -> T
            def isA = (_).(a).a
            
            sig one : int | char
            def one = inl (isA int 1)
        """.trimIndent().let { check(it) }
    }

}) {
    companion object {
        fun check(program: String) =
            (ParserStage() compile program).let { bindings ->
                AbstractionStage() compile bindings
            }.let { bindings ->
                CheckerStage<Region.T>(invoke(program)) compile bindings
            }
    }
}