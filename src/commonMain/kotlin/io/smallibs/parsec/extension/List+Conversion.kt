package io.smallibs.parsec.extension

fun List<Char>.charsToString(separator: String = ""): String = this.toCharArray().joinToString(separator = separator)
fun List<String>.stringsToString(separator: String = ""): String = this.joinToString(separator = separator)
fun List<Char>.charsToInt(): Int = this.charsToString().toInt()
fun List<Char>.charsToFloat(): Float = this.charsToString().toFloat()
