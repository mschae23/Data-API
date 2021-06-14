package de.martenschaefer.data.serialization

import com.google.gson.{ Gson, GsonBuilder, JsonElement, JsonParser }
import de.martenschaefer.data.serialization.codec.GsonElementCodec
import de.martenschaefer.data.serialization.{ Decoder, Element, Encoder }
import de.martenschaefer.data.util.Either
import de.martenschaefer.data.util.Either._

object JsonCodecs {
    private val gson = new GsonBuilder().create()
    private val prettyGson = new GsonBuilder().setPrettyPrinting().create()

    given jsonEncoder: Encoder[Element, String] with {
        override def encode(value: Element): String =
            GsonElementCodec.decodeElement(value).map(gson.toJson(_)) match {
                case Right(json) => json
                case Left(errors) => throw new IllegalArgumentException(errors.toString)
            }
    }

    val prettyJsonEncoder = new Encoder[Element, String] {
        override def encode(value: Element): String =
            GsonElementCodec.decodeElement(value).map(prettyGson.toJson(_)) match {
                case Right(json) => json
                case Left(errors) => throw new IllegalArgumentException(errors.toString)
            }
    }

    given jsonDecoder: Decoder[Element, String] with {
        override def decode(encoded: String): Decoded[Element] = {
            Right(GsonElementCodec.encodeElement(JsonParser.parseString(encoded)))
        }
    }
}
