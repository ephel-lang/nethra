package io.smallibs.lang.nethra.stages.common

interface Stage<I, O> {
    infix fun act(i: I): O
}