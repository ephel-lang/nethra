package io.smallibs.parsec.parser

object Common {
    internal fun <I, A> Response<I, A>.success(): Response.Accept<I, A>? = this.fold({ it }, { null })

    internal fun <I, A> Response<I, A>.error(): Response.Reject<I, A>? = this.fold({ null }, { it })

    internal fun <I, A> Response<I, A>.get(): A? = this.success()?.value

    internal fun <I, A> Response<I, A>.isSuccess(): Boolean = this.success() != null
}