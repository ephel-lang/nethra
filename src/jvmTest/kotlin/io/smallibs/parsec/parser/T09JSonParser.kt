package io.smallibs.parsec.parser

import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe
import io.smallibs.parsec.io.Reader
import io.smallibs.parsec.parser.Flow.eos
import io.smallibs.parsec.parser.Flow.thenLeft
import io.smallibs.parsec.parser.json.Json.JsonArray
import io.smallibs.parsec.parser.json.Json.JsonBoolean
import io.smallibs.parsec.parser.json.Json.JsonNull
import io.smallibs.parsec.parser.json.Json.JsonNumber
import io.smallibs.parsec.parser.json.Json.JsonObject
import io.smallibs.parsec.parser.json.Json.JsonString
import io.smallibs.parsec.parser.json.JsonParser
import io.smallibs.parsec.parser.json.JsonParser.JSON
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext


class T09JSonParser : StringSpec({

    "should JsonParser Return Integer" {
        JSON.invoke(Reader.string("42")).fold({ it.value == JsonNumber(42F) }, { false }) shouldBe true
    }

    "should JsonParser Return Null" {
        JSON.invoke(Reader.string("null")).fold({ it.value == JsonNull }, { false }) shouldBe true
    }

    "should JsonParser Return True" {
        JSON.invoke(Reader.string("true")).fold({ it.value == JsonBoolean(true) }, { false }) shouldBe true
    }

    "should JsonParser Return False" {
        JSON.invoke(Reader.string("false")).fold({ it.value == JsonBoolean(false) }, { false }) shouldBe true
    }

    "should JsonParser Return String" {
        JSON.invoke(Reader.string("\"42\"")).fold({ it.value == JsonString("42") }, { false }) shouldBe true
    }

    "should JsonParser Return Empty Array" {
        JSON.invoke(Reader.string("[]")).fold({ it.value == JsonArray(listOf()) }, { false }) shouldBe true
    }

    "should JsonParser Return Empty Object" {
        JSON.invoke(Reader.string("{}")).fold({ it.value == JsonObject(mapOf()) }, { false }) shouldBe true
    }

    "should JsonParser Return Singleton Object" {
        JSON.invoke(Reader.string("{\"a\":42}"))
            .fold({ it.value == JsonObject(mapOf("a" to JsonNumber(42F))) }, { false }) shouldBe true
    }

    "should JsonParser Return Singleton Array" {
        JSON.invoke(Reader.string("[42]"))
            .fold({ it.value == JsonArray(listOf(JsonNumber(42F))) }, { false }) shouldBe true
    }

    "should JsonParser Return Array" {
        JSON.invoke(Reader.string("[42,43]"))
            .fold({ it.value == JsonArray(listOf(JsonNumber(42F), JsonNumber(43F))) }, { false }) shouldBe true
    }

    "should Parse Json File" {
        val stream = T09JSonParser::class.java.getResourceAsStream("/fragment.Json")
        val reader = JsonParser.reader(Reader.string(String(withContext(Dispatchers.IO) {
            stream.readAllBytes()
        })))

        JSON.thenLeft(eos()).invoke(reader).fold({ true }, { false }) shouldBe true
    }
})