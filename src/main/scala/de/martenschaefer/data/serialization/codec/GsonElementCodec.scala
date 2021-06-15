package de.martenschaefer.data.serialization.codec

import scala.jdk.CollectionConverters.{ IterableHasAsScala, SetHasAsScala }
import com.google.gson.internal.LazilyParsedNumber
import com.google.gson.{ JsonArray, JsonElement, JsonNull, JsonObject, JsonPrimitive }
import de.martenschaefer.data.serialization.Element._
import de.martenschaefer.data.serialization.{ Codec, Element, ElementError, ElementNode, Result }
import de.martenschaefer.data.util.Either._
import de.martenschaefer.data.util.{ Either, Lifecycle, Utils }

object GsonElementCodec extends Codec[JsonElement] {
    override def encodeElement(json: JsonElement): Result[Element] = json match {
        case _ if json.isJsonNull => Right(Null)
        case _ if json.isJsonPrimitive => Right(json.getAsJsonPrimitive match {
            case p if p.isNumber => p.getAsNumber match {
                case n: LazilyParsedNumber => Utils.parsePrimitive(n.toString)
                case i: Int => IntElement(i.intValue())
                case l: Long => LongElement(l.longValue())
                case f: Float => FloatElement(f.floatValue())
                case d: Double => DoubleElement(d.doubleValue())
            }
            case p if p.isBoolean => BooleanElement(p.getAsBoolean)
            case p if p.isString => StringElement(p.getAsString)
        })
        case _ if json.isJsonArray => val array = json.getAsJsonArray
            var result: Result[List[Element]] = Right(List())

            for (i <- 0 until array.size) {
                val element = array.get(i)
                this.encodeElement(element).mapBoth(errors => result = result.flatMapBoth(errorList => Left(errorList
                    .appendedAll(Utils.withPrependedPath(errors, ElementNode.Index(i)))))(
                    _ => Left(Utils.withPrependedPath(errors, ElementNode.Index(i)))))(e =>
                    result = result.map(_.appended(e))
                )
            }

            result match {
                case Right(elements) => Right(ArrayElement(elements))
                case Left(errors) => Left(errors)
            }

        case _ if json.isJsonObject => val jsonObject = json.getAsJsonObject
            var result: Result[Map[String, Element]] = Right(Map())

            for ((key, element) <- jsonObject.entrySet.asScala.map(entry => (entry.getKey, entry.getValue))) {
                this.encodeElement(element).mapBoth(errors => result = result.flatMapBoth(errorList => Left(errorList
                    .appendedAll(Utils.withPrependedPath(errors, key))))(_ => Left(Utils.withPrependedPath(errors, key))))(
                    e => result = result.map(_ + (key -> e)))
            }

            result match {
                case Right(map) => Right(ObjectElement(map))
                case Left(errors) => Left(errors)
            }
    }

    override def decodeElement(element: Element): Result[JsonElement] = element match {
        case Null | None => Right(JsonNull.INSTANCE)
        case IntElement(value) => Right(new JsonPrimitive(value))
        case LongElement(value) => Right(new JsonPrimitive(value))
        case FloatElement(value) => Right(new JsonPrimitive(value))
        case DoubleElement(value) => Right(new JsonPrimitive(value))
        case BooleanElement(value) => Right(new JsonPrimitive(value))
        case StringElement(value) => Right(new JsonPrimitive(value))

        case ArrayElement(values) => val array = new JsonArray(values.size)
            for (i <- 0 until values.size)
                val value = values(i)
                this.decodeElement(value).mapBoth(errors => return Left(errors.map(_.withPrependedPath(
                    ElementNode.Index(i)))))(array.add(_))
            Right(array)
        case ObjectElement(map) => val json = new JsonObject()
            for ((key, e) <- map)
                this.decodeElement(e).mapBoth(errors => return Left(errors.map(_.withPrependedPath(
                    key))))(value => json.add(key, value))
            Right(json)
    }

    override val lifecycle: Lifecycle = Lifecycle.Experimental
}
