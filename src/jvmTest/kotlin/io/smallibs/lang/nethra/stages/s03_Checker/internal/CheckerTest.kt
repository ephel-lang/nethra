package io.smallibs.lang.nethra.stages.s03_Checker.internal

import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe
import io.smallibs.lang.nethra.ast.Builder
import io.smallibs.lang.extension.Multi

class CheckerTest : StringSpec({
    Multi.with(Builder<Nothing>(), Checker<Nothing>()) {
        {
            " ✅ Γ |- Type_i : Type_{i+1}" {
                Context<Nothing>().check(type(3), type(4)) shouldBe true
            }

            " ❌ Γ |- Type_i : Type_{j} (with j != i+1)" {
                Context<Nothing>().check(type(3), type(5)) shouldBe false
            }

            " ✅ Γ |- (data x : T) : T" {
                Context<Nothing>().check(data("x", id("T")), id("T")) shouldBe true
            }

            " ❌ Γ |- (data x : S) : T" {
                Context<Nothing>().check(data("x", id("S")), id("T")) shouldBe false
            }

            " ✅ Γ, x : T |- x : T" {
                Context<Nothing>().setSignature("x", id("T")).check(id("x"), id("T")) shouldBe true
            }

            " ❌ Γ, x : T |- x : S" {
                Context<Nothing>().setSignature("x", id("T")).check(id("x"), id("S")) shouldBe false
            }

            " ✅ Γ |- 1 : int" {
                Context<Nothing>().check(int(1), data("int", type())) shouldBe true
            }

            " ❌ Γ |- 1 : char" {
                Context<Nothing>().check(int(1), data("char", type())) shouldBe false
            }

            " ✅ Γ |- Π(x:int).int : Type_0" {
                val int = data("int", type())
                Context<Nothing>().check(pi("x", int, int), type()) shouldBe true
            }

            " ❌ Γ |- Π(x:int).type : Type_0" {
                val int = data("int", type())
                Context<Nothing>().check(pi("x", int, type()), type()) shouldBe false
            }

            " ✅ Γ |- λ(x).x : Π(x:int).int" {
                val int = data("int", type())
                Context<Nothing>().check(lambda("y", id("y")), pi("x", int, int)) shouldBe true
            }

            " ✅ Γ |- λ(x).x : int → int" {
                val int = data("int", type())
                Context<Nothing>().check(lambda("y", id("y")), int arrow int) shouldBe true
            }

            " ✅ Γ |- λ(x).int : Π(x:int).Type_0" {
                val int = data("int", type())
                Context<Nothing>().check(lambda("x", int), pi("x", int, type())) shouldBe true
            }

            " ✅ Γ |- (λ(x).x :: int -> int) 1 : int" {
                val int = data("int", type())
                val func = lambda("x", id("x"))
                val type = int arrow int
                Context<Nothing>().check(apply(inhabit(func, type), int(1)), int) shouldBe true
            }

            " ✅ Γ |- (λ(_)(x).x :: Π(t:Type).t -> t) int 1 : int" {
                val int = data("int", type())
                val func = lambda("_", lambda("x", id("x")))
                val type = pi("t", type(), id("t") arrow id("t"))
                Context<Nothing>().check(apply(apply(inhabit(func, type), int), int(1)), int) shouldBe true
            }

            " ✅ Γ |- (λ{_}(x).x :: Π{t:Type}.t -> t) {int} 1 : int" {
                val int = data("int", type())
                val func = lambda("_", lambda("x", id("x")), true)
                val type = pi("t", type(), id("t") arrow id("t"), true)
                Context<Nothing>().check(apply(apply(inhabit(func, type), int, true), int(1)), int) shouldBe true
            }

            " ✅ Γ |- (λ{_}(x).x :: Π{t:Type}.t -> t) 1 : int" {
                val int = data("int", type())
                val func = lambda("_", lambda("x", id("x")), true)
                val type = pi("t", type(), id("t") arrow id("t"), true)
                Context<Nothing>().check(apply(inhabit(func, type), int(1)), int) shouldBe true
            }

            " ✅ Γ |- (λ{_}(x).x :: Π{t1:Type}.Π{t2:Type}.t2 -> t2) 1 : int" {
                val int = data("int", type())
                val func = lambda("_", lambda("_", lambda("x", id("x")), true), true)
                val type = pi("t1", type(), pi("t2", type(), id("t2") arrow id("t2"), true), true)
                Context<Nothing>().check(apply(inhabit(func, type), int(1)), int) shouldBe true
            }

            " ✅ Γ |- λ(x).x 1 : int" {
                val int = data("int", type())
                val func = lambda("x", id("x"))
                Context<Nothing>().check(apply(func, int(1)), int) shouldBe true
            }

            " ✅ Γ |- λ{_}.λ(x).x 1 : int" {
                val int = data("int", type())
                val func = lambda("_", lambda("x", id("x")), true)
                Context<Nothing>().check(apply(func, int(1)), int) shouldBe true
            }

            " ❌ Γ |- (λ(x).x :: int -> char) 1 : int" {
                val int = data("int", type())
                val char = data("char", type())
                Context<Nothing>().check(apply(inhabit(lambda("x", id("x")), pi("_", int, char)), int(1)),
                    int) shouldBe false
            }

            " ✅ Γ |- Σ(x:int).int : Type_0" {
                val int = data("int", type())
                Context<Nothing>().check(sigma("x", int, int), type()) shouldBe true
            }

            " ❌ Γ |- Σ(x:int).Type_0 : Type_0" {
                val int = data("int", type())
                Context<Nothing>().check(sigma("x", int, type()), type()) shouldBe false
            }

            " ✅ Γ |- int,2 : Σ(x:Type_0).x" {
                val int = data("int", type())
                Context<Nothing>().check(pair(int, int(2)), sigma("x", type(), id("x"))) shouldBe true
            }

            " ✅ Γ |- 1,2 : int*int" {
                val int = data("int", type())
                Context<Nothing>().check(pair(int(1), int(2)), int and int) shouldBe true
            }

            " ❌ Γ |- char,2 : Σ(x:Type_0).x" {
                val char = data("char", type())
                Context<Nothing>().check(pair(char, int(2)), sigma("x", type(), id("x"))) shouldBe false
            }

            " ✅ Γ |- fst (int,2) : Type_0" {
                val int = data("int", type())
                Context<Nothing>().check(fst(pair(int, int(2))), type()) shouldBe true
            }

            " ✅ Γ, x : Σ(x:Type_0).x |- fst x : Type_0" {
                Context<Nothing>().setSignature("x", sigma("x", type(), id("x"))).check(fst(id("x")), type()) shouldBe true
            }

            " ❌ Γ |- fst 1 : T" {
                Context<Nothing>().check(fst(int(1)), id("T")) shouldBe false
            }

            " ✅ Γ |- snd (int,2) : int" {
                val int = data("int", type())
                Context<Nothing>().check(snd(pair(int, int(2))), int) shouldBe true
            }

            " ✅ Γ, y : Σ(x:Type_0).x |- snd y : fst y" {
                Context<Nothing>().setSignature("y", sigma("x", type(), id("x"))).check(snd(id("y")), fst(id("y"))) shouldBe true
            }

            " ❌ Γ |- snd 1 : T" {
                Context<Nothing>().check(snd(int(1)), id("T")) shouldBe false
            }

            " ✅ Γ |- int + char : Type_0" {
                val int = data("int", type())
                val char = data("char", type())
                Context<Nothing>().check(or(int, char), type()) shouldBe true
            }

            " ❌ Γ |- 1 + char : Type_0" {
                val char = data("char", type())
                Context<Nothing>().check(or(int(1), char), type()) shouldBe false
            }

            " ✅ Γ |- inl 1 : int + char" {
                val int = data("int", type())
                val char = data("char", type())
                Context<Nothing>().check(inl(int(1)), or(int, char)) shouldBe true
            }

            " ❌ Γ |- inl 1 : char + int" {
                val int = data("int", type())
                val char = data("char", type())
                Context<Nothing>().check(inl(int(1)), or(char, int)) shouldBe false
            }

            " ✅ Γ |- inr 'a' : int + char" {
                val int = data("int", type())
                val char = data("char", type())
                Context<Nothing>().check(inr(char('1')), or(int, char)) shouldBe true
            }

            " ❌ Γ |- inr 'a' : char + int" {
                val int = data("int", type())
                val char = data("char", type())
                Context<Nothing>().check(inr(char('1')), or(char, int)) shouldBe false
            }

            " ✅ Γ, e : A + B |- case e λ(_).0 λ(_).1 : int" {
                val int = data("int", type())
                val term = id("e")
                val type = or(data("A", type()), data("B", type()))
                val left = lambda("_", int(0))
                val right = lambda("_", int(1))
                Context<Nothing>().setSignature("e", type).check(case(term, left, right), int) shouldBe true
            }

            " ❌ Γ, e : A + B |- case e λ(_).0 λ(_).'1' : int" {
                val int = data("int", type())
                val term = id("e")
                val type = or(data("A", type()), data("B", type()))
                val left = lambda("_", int(0))
                val right = lambda("_", char('1'))
                Context<Nothing>().setSignature("e", type).check(case(term, left, right), int) shouldBe false
            }

            " ✅ Γ, e : A + B |- case e λ(_).(inl 0) λ(_).(inr '1') : int + char" {
                val intOrChar = or(data("int", type()), data("char", type()))
                val term = id("e")
                val type = or(data("A", type()), data("B", type()))
                val left = lambda("_", inl(int(0)))
                val right = lambda("_", inr(char('1')))
                Context<Nothing>().setSignature("e", type).check(case(term, left, right), intOrChar) shouldBe true
            }

            " ✅ Γ |- μ(x).(x -> int) : Type_0" {
                val int = data("int", type())
                Context<Nothing>().check(rec("x", id("x") arrow int), type()) shouldBe true
            }

            " ✅ Γ, a : μ(x).(x -> int) -> int |- fold μ(x).(x -> int) a : μ(x).(x -> int)" {
                val int = data("int", type())
                val rec = rec("x", id("x") arrow int)
                val unfolded = rec arrow int
                Context<Nothing>().setSignature("a", unfolded).check(fold(id("a"), rec), rec) shouldBe true
            }

            " ✅ Γ, a : μ(x).(x -> int) |- unfold μ(x).(x -> int) a  : μ(x).(x -> int) -> int" {
                val int = data("int", type())
                val rec = rec("x", id("x") arrow int)
                val unfolded = rec arrow int
                Context<Nothing>().setSignature("a", rec).check(unfold(id("a"), rec), unfolded) shouldBe true
            }
        }
    }
})