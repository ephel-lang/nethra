package io.smallibs.parsec.parser

object Common {
    internal fun <A> Response<*, A>.get(): A? = this.fold({ it.value }, { null })

    internal fun <A> Response<*, A>.isSuccess(): Boolean = this.fold({ true }, { false })
}