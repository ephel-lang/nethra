package io.smallibs.lang.nethra.stages.common

interface Stage<I, O> {
    infix fun compile(i: I): O
    infix fun decompile(o: O): I
}