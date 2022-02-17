package io.smallibs.parsec.extension

infix fun <A, B, C> ((B) -> C).compose(f: (A) -> B): (A) -> C = { this(f(it)) }
infix fun <A, B, C> ((A) -> B).pipe(f: (B) -> C): (A) -> C = f compose this
