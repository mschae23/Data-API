package de.martenschaefer.data.serialization.codec

import scala.jdk.CollectionConverters.{ IterableHasAsScala, SetHasAsScala }
import com.google.gson.internal.LazilyParsedNumber
import com.google.gson.{ JsonArray, JsonElement, JsonNull, JsonObject, JsonPrimitive }
import de.martenschaefer.data.serialization.Element._
import de.martenschaefer.data.serialization.{ Codec, Decoded, Element, ElementError, ElementNode }
import de.martenschaefer.data.util.Either._
import de.martenschaefer.data.util.{ Either, Lifecycle, Utils }

object GsonElementCodec extends Codec[JsonElement] {
    override def encodeElement(json: JsonElement): Element = json match {
        case _ if json.isJsonNull => Null
        case _ if json.isJsonPrimitive => json.getAsJsonPrimitive match {
            case p if p.isNumber => p.getAsNumber match {
                case n: LazilyParsedNumber => Utils.parsePrimitive(n.toString)
                case i: Int => IntElement(i.intValue())
                case l: Long => LongElement(l.longValue())
                case f: Float => FloatElement(f.floatValue())
                case d: Double => DoubleElement(d.doubleValue())
            }
            case p if p.isBoolean => BooleanElement(p.getAsBoolean)
            case p if p.isString => StringElement(p.getAsString)
        }
        case _ if json.isJsonArray => ArrayElement(List.from(json.getAsJsonArray.asScala).map(this.encodeElement(_)))
        case _ if json.isJsonObject => ObjectElement(Map.from(json.getAsJsonObject.entrySet.asScala
            .map(entry => (entry.getKey, entry.getValue))).map((key, element) => (key, this.encodeElement(element))))
    }

    override def decodeElement(element: Element): Decoded[JsonElement] = element match {
        case Null | None => Right(JsonNull.INSTANCE)
        case IntElement(value) => Right(new JsonPrimitive(value))
        case LongElement(value) => Right(new JsonPrimitive(value))
        case FloatElement(value) => Right(new JsonPrimitive(value))
        case DoubleElement(value) => Right(new JsonPrimitive(value))
        case BooleanElement(value) => Right(new JsonPrimitive(value))
        case StringElement(value) => Right(new JsonPrimitive(value))

        case ArrayElement(values) => val array = new JsonArray(values.size)
            for (i <- 0 until values.size) {
                val value = values(i)
                this.decodeElement(value).mapBoth(errors => return Left(errors.map(_.withPrependedPath(
                    ElementNode.Index(i)))))(array.add(_))
            }
            Right(array)
        case ObjectElement(map) => val json = new JsonObject()
            for ((key, e) <- map)
                this.decodeElement(e).mapBoth(errors => return Left(errors.map(_.withPrependedPath(
                    key))))(value => json.add(key, value))
            Right(json)
    }

    override val lifecycle: Lifecycle = Lifecycle.Experimental
}
