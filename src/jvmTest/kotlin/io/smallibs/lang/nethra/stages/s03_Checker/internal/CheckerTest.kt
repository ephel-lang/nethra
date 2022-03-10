package io.smallibs.lang.nethra.stages.s03_Checker.internal

import io.kotest.assertions.throwables.shouldThrowExactly
import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe
import io.smallibs.lang.extension.Multi
import io.smallibs.lang.nethra.ast.Ast.Term.Companion.ANON
import io.smallibs.lang.nethra.ast.Builder
import io.smallibs.lang.nethra.stages.report.CompilationException

class CheckerTest : StringSpec({
    Multi.with(Builder<Nothing>(), Checker<Nothing>()) {
        {
            " ✅ Γ |- Type_i : Type_{i+1}" {
                Bindings<Nothing>().check(type(3), type(4)).success() shouldBe true
            }

            " ❌ Γ |- Type_i : Type_{j} (with j <= i)" {
                shouldThrowExactly<CompilationException.CannotCompare> {
                    Bindings<Nothing>().check(type(3), type(3)).error()
                }
            }

            " ✅ Γ, x : T |- x : T" {
                Bindings<Nothing>().setSignature("x", id("T")).check(id("x"), id("T")).success() shouldBe true
            }

            " ❌ Γ, x : T |- x : S" {
                shouldThrowExactly<CompilationException.CannotCompare> {
                    Bindings<Nothing>().setSignature("x", id("T")).check(id("x"), id("S")).error()
                }
            }

            " ✅ Γ |- 1 : int" {
                Bindings<Nothing>().check(int(1), id("int")).success() shouldBe true
            }

            " ❌ Γ |- 1 : char" {
                shouldThrowExactly<CompilationException.CannotCompare> {
                    Bindings<Nothing>().check(int(1), id("char")).error()
                }
            }

            " ✅ Γ |- Π(x:int).int : Type_0" {
                val int = id("int")
                Bindings<Nothing>().check(pi("x", int, int), type()).success() shouldBe true
            }

            " ❌ Γ |- Π(x:int).type : Type_0" {
                shouldThrowExactly<CompilationException.CannotCompare> {
                    val int = id("int")
                    Bindings<Nothing>().check(pi("x", int, type()), type()).error()
                }
            }

            " ✅ Γ |- λ(x).x : Π(x:int).int" {
                val int = id("int")
                Bindings<Nothing>().check(lambda("y", id("y")), pi("x", int, int)).success() shouldBe true
            }

            " ✅ Γ |- λ(x).x : int → int" {
                val int = id("int")
                Bindings<Nothing>().check(lambda("y", id("y")), int arrow int).success() shouldBe true
            }

            " ✅ Γ |- λ(x).int : Π(x:int).Type_0" {
                val int = id("int")
                Bindings<Nothing>().check(lambda("x", int), pi("x", int, type())).success() shouldBe true
            }

            " ✅ Γ |- (λ(x).x :: int -> int) 1 : int" {
                val int = id("int")
                val func = lambda("x", id("x"))
                Bindings<Nothing>().check(apply(func, int(1)), int).success() shouldBe true
            }

            " ✅ Γ |- (λ(_).λ(x).x :: Π(t:Type).t -> t) int 1 : int" {
                val int = id("int")
                val func = lambda(ANON, lambda("x", id("x")))
                Bindings<Nothing>().check(apply(apply(func, int), int(1)), int).success() shouldBe true
            }

            " ✅ Γ |- (λ{_}.λ(x).x :: Π{t:Type}.t -> t) {int} 1 : int" {
                val int = id("int")
                val func = lambda(ANON, lambda("x", id("x")), true)
                Bindings<Nothing>().check(apply(apply(func, int, true), int(1)), int).success() shouldBe true
            }

            " ✅ Γ |- (λ{_}.λ(x).x :: Π{t:Type}.t -> t) 1 : int" {
                val int = id("int")
                val func = lambda(ANON, lambda("x", id("x")), true)
                Bindings<Nothing>().check(apply(func, int(1)), int).success() shouldBe true
            }

            " ✅ Γ |- (λ{_}.λ(x).x :: Π{t2:Type}.t2 -> t2) 1 : int" {
                val int = id("int")
                val func = lambda(ANON, lambda("x", id("x")), true)
                Bindings<Nothing>().check(apply(func, int(1)), int).success() shouldBe true
            }

            " ✅ Γ |- (λ(x).x :: Π{t2:Type}.t2 -> t2) 1 : int" {
                val int = id("int")
                val func = lambda("x", id("x"))
                Bindings<Nothing>().check(apply(func, int(1)), int).success() shouldBe true
            }

            " ✅ Γ |- λ(x).x 1 : int" {
                val int = id("int")
                val func = lambda("x", id("x"))
                Bindings<Nothing>().check(apply(func, int(1)), int).success() shouldBe true
            }

            " ✅ Γ |- λ{_}.λ(x).x 1 : int" {
                val int = id("int")
                val func = lambda(ANON, lambda("x", id("x")), true)
                Bindings<Nothing>().check(apply(func, int(1)), int).success() shouldBe true
            }

            " ❌ Γ |- λ(x).x 1 : char" {
                shouldThrowExactly<CompilationException.CannotCompare> {
                    val char = id("char")
                    Bindings<Nothing>().check(apply(lambda("x", id("x")), int(1)), char).error()
                }
            }

            " ✅ Γ |- Σ(x:int).int : Type_0" {
                val int = id("int")
                Bindings<Nothing>().check(sigma("x", int, int), type()).success() shouldBe true
            }

            " ❌ Γ |- Σ(x:int).Type_0 : Type_0" {
                shouldThrowExactly<CompilationException.CannotCompare> {
                    val int = id("int")
                    Bindings<Nothing>().check(sigma("x", int, type()), type()).error()
                }
            }

            " ✅ Γ |- int,2 : Σ(x:Type_0).x" {
                val int = id("int")
                Bindings<Nothing>().check(pair(int, int(2)), sigma("x", type(), id("x"))).success() shouldBe true
            }

            " ✅ Γ |- 1,2 : int*int" {
                val int = id("int")
                Bindings<Nothing>().check(pair(int(1), int(2)), int and int).success() shouldBe true
            }

            " ❌ Γ |- char,2 : Σ(x:Type_0).x" {
                shouldThrowExactly<CompilationException.CannotCompare> {
                    val char = id("char")
                    Bindings<Nothing>().check(pair(char, int(2)), sigma("x", type(), id("x"))).error()
                }
            }

            " ✅ Γ |- fst (int,2) : Type_0" {
                val int = id("int")
                Bindings<Nothing>().check(fst(pair(int, int(2))), type()).success() shouldBe true
            }

            " ✅ Γ, x : Σ(x:Type_0).x |- fst x : Type_0" {
                Bindings<Nothing>().setSignature("x", sigma("x", type(), id("x")))
                    .check(fst(id("x")), type()).success() shouldBe true
            }

            " ❌ Γ |- fst 1 : T" {
                shouldThrowExactly<CompilationException.CannotCheck> {
                    Bindings<Nothing>().check(fst(int(1)), id("T")).error()
                }
            }

            " ✅ Γ |- snd (int,2) : int" {
                val int = id("int")
                Bindings<Nothing>().check(snd(pair(int, int(2))), int).success() shouldBe true
            }

            " ✅ Γ, y : Σ(x:Type_0).x |- snd y : fst y" {
                Bindings<Nothing>().setSignature("y", sigma("x", type(), id("x")))
                    .check(snd(id("y")), fst(id("y"))).success() shouldBe true
            }

            " ❌ Γ |- snd 1 : T" {
                shouldThrowExactly<CompilationException.CannotCheck> {
                    Bindings<Nothing>().check(snd(int(1)), id("T")).error()
                }
            }

            " ✅ Γ |- int + char : Type_0" {
                val int = id("int")
                val char = id("char")
                Bindings<Nothing>().check(or(int, char), type()).success() shouldBe true
            }

            " ❌ Γ |- 1 + char : Type_0" {
                shouldThrowExactly<CompilationException.CannotCompare> {
                    val char = id("char")
                    Bindings<Nothing>().check(or(int(1), char), type()).error()
                }
            }

            " ✅ Γ |- inl 1 : int + char" {
                val int = id("int")
                val char = id("char")
                Bindings<Nothing>().check(inl(int(1)), or(int, char)).success() shouldBe true
            }

            " ❌ Γ |- inl 1 : char + int" {
                shouldThrowExactly<CompilationException.CannotCompare> {
                    val int = id("int")
                    val char = id("char")
                    Bindings<Nothing>().check(inl(int(1)), or(char, int)).error()
                }
            }

            " ✅ Γ |- inr 'a' : int + char" {
                val int = id("int")
                val char = id("char")
                Bindings<Nothing>().check(inr(char('1')), or(int, char)).success() shouldBe true
            }

            " ❌ Γ |- inr 'a' : char + int" {
                shouldThrowExactly<CompilationException.CannotCompare> {
                    val int = id("int")
                    val char = id("char")
                    Bindings<Nothing>().check(inr(char('1')), or(char, int)).error()
                }
            }

            " ✅ Γ, e : A + B |- case e λ(_).0 λ(_).1 : int" {
                val int = id("int")
                val term = id("e")
                val type = or(id("A"), id("B"))
                val left = lambda(ANON, int(0))
                val right = lambda(ANON, int(1))
                Bindings<Nothing>().setSignature("e", type).check(case(term, left, right), int).success() shouldBe true
            }

            " ❌ Γ, e : A + B |- case e λ(_).0 λ(_).'1' : int" {
                shouldThrowExactly<CompilationException.CannotCompare> {
                    val int = id("int")
                    val term = id("e")
                    val type = or(id("A"), id("B"))
                    val left = lambda(ANON, int(0))
                    val right = lambda(ANON, char('1'))
                    Bindings<Nothing>().setSignature("e", type).check(case(term, left, right), int).error()
                }
            }

            " ✅ Γ, e : A + B |- case e λ(_).(inl 0) λ(_).(inr '1') : int + char" {
                val intOrChar = or(id("int"), id("char"))
                val term = id("e")
                val type = or(id("A"), id("B"))
                val left = lambda(ANON, inl(int(0)))
                val right = lambda(ANON, inr(char('1')))
                Bindings<Nothing>().setSignature("e", type).check(case(term, left, right), intOrChar).success() shouldBe true
            }

            " ✅ Γ |- μ(x).(x -> int) : Type_0" {
                val int = id("int")
                Bindings<Nothing>().check(rec("x", id("x") arrow int), type()).success() shouldBe true
            }

            " ✅ Γ, a : μ(x).(x -> int) -> int |- fold μ(x).(x -> int) a : μ(x).(x -> int)" {
                val int = id("int")
                val rec = rec("x", id("x") arrow int)
                val unfolded = rec arrow int
                Bindings<Nothing>().setSignature("a", unfolded).check(fold(id("a")), rec).success() shouldBe true
            }

            " ✅ Γ, a : μ(x).(x -> int) |- unfold μ(x).(x -> int) a  : μ(x).(x -> int) -> int" {
                val int = id("int")
                val rec = rec("x", id("x") arrow int)
                val unfolded = rec arrow int
                Bindings<Nothing>().setSignature("a", rec).check(unfold(id("a")), unfolded).success() shouldBe true
            }
        }
    }
})
