package io.smallibs.lang.extension

object Multi {
    fun <T, R, O> with(t: T, r: R, b: T.() -> R.() -> O): O = r.(t.b())()
}