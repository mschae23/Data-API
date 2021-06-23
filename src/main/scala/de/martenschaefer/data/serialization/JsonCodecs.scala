package de.martenschaefer.data.serialization

import com.google.gson.{ Gson, GsonBuilder, JsonElement, JsonParser }
import de.martenschaefer.data.serialization.codec.GsonElementCodec
import de.martenschaefer.data.serialization.{ Decoder, Element, Encoder }
import de.martenschaefer.data.util.DataResult
import de.martenschaefer.data.util.DataResult._

object JsonCodecs {
    private val gson = new GsonBuilder().create()
    private val prettyGson = new GsonBuilder().setPrettyPrinting().create()

    given jsonEncoder: Encoder[Element, String] with {
        override def encode(value: Element): Result[String] =
            GsonElementCodec.decodeElement(value).map(gson.toJson(_))
    }

    val prettyJsonEncoder = new Encoder[Element, String] {
        override def encode(value: Element): Result[String] =
            GsonElementCodec.decodeElement(value).map(prettyGson.toJson(_))
    }

    given jsonDecoder: Decoder[Element, String] with {
        override def decode(encoded: String): Result[Element] = {
            GsonElementCodec.encodeElement(JsonParser.parseString(encoded))
        }
    }
}
