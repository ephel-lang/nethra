package io.smallibs.parsec.parser.json

sealed interface Json {
    object JsonNull : Json
    data class JsonBoolean(val v: Boolean) : Json
    data class JsonNumber(val v: Float) : Json
    data class JsonString(val v: String) : Json
    data class JsonArray(val v: List<Json>) : Json
    data class JsonObject(val v: Map<String, Json>) : Json
}