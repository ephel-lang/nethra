package io.smallibs.lang.nethra.ast

import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe
import io.smallibs.lang.extension.Multi

class PrinterTest : StringSpec({
    Multi.with(Builder<Nothing>(), Printer<Nothing>()) {
        {
            "should pretty type0" {
                type().prettyPrint() shouldBe "Type_0"
            }

            "should pretty type1" {
                type(1).prettyPrint() shouldBe "Type_1"
            }

            "should pretty constructor" {
                data("nil", id("nil")).prettyPrint() shouldBe "nil"
            }

            "should pretty id" {
                id("nil").prettyPrint() shouldBe "nil"
            }

            "should pretty Π" {
                pi("x", type(), id("x")).prettyPrint() shouldBe "Π(x:Type_0).(x)"
            }

            "should pretty implicit Π" {
                pi("x", type(), id("x"), true).prettyPrint() shouldBe "Π{x:Type_0}.(x)"
            }

            "should pretty λ" {
                lambda("x", id("x")).prettyPrint() shouldBe "λ(x).(x)"
            }

            "should pretty implicit λ" {
                lambda("x", id("x"), true).prettyPrint() shouldBe "λ{x}.(x)"
            }

            "should pretty apply" {
                apply(type(), id("x")).prettyPrint() shouldBe "Type_0 (x)"
            }

            "should pretty implicit apply" {
                apply(type(), id("x"), true).prettyPrint() shouldBe "Type_0 ({x})"
            }

            "should pretty Σ" {
                sigma("x", type(), id("x")).prettyPrint() shouldBe "Σ(x:Type_0).(x)"
            }

            "should pretty couple" {
                pair(type(), id("x")).prettyPrint() shouldBe "(Type_0,x)"
            }

            "should pretty fst" {
                fst(id("x")).prettyPrint() shouldBe "fst x"
            }

            "should pretty snd" {
                snd(id("x")).prettyPrint() shouldBe "snd x"
            }

            "should pretty disjunction" {
                or(type(), id("x")).prettyPrint() shouldBe "Type_0 + x"
            }

            "should pretty inl" {
                inl(id("x")).prettyPrint() shouldBe "inl x"
            }

            "should pretty inr" {
                inr(id("x")).prettyPrint() shouldBe "inr x"
            }

            "should pretty recursion" {
                rec("x",
                    lambda("y", apply(id("x"), id("y")))).prettyPrint() shouldBe "μ(x).(λ(y).(x (y)))"
            }

            "should pretty inhabitation" {
                inhabit(type(), type(1)).prettyPrint() shouldBe "(Type_0 ∈ Type_1)"
            }
        }
    }
})
