package io.smallibs.parsec.parser

import io.smallibs.parsec.io.Reader

typealias Parser<I, A> = (Reader<I>) -> Response<I, A>
