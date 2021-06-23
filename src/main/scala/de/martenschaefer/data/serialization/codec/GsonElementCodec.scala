package de.martenschaefer.data.serialization.codec

import scala.jdk.CollectionConverters.{ IterableHasAsScala, SetHasAsScala }
import com.google.gson.internal.LazilyParsedNumber
import com.google.gson.{ JsonArray, JsonElement, JsonNull, JsonObject, JsonPrimitive }
import de.martenschaefer.data.serialization.Element._
import de.martenschaefer.data.serialization.{ Codec, Element, ElementError, ElementNode, ParseError, RecordParseError, Result }
import de.martenschaefer.data.util.DataResult._
import de.martenschaefer.data.util.{ DataResult, Lifecycle, Utils }

object GsonElementCodec extends Codec[JsonElement] {
    override def encodeElement(json: JsonElement): Result[Element] = json match {
        case _ if json.isJsonNull => Success(Null, Lifecycle.Stable)
        case _ if json.isJsonPrimitive => Success(json.getAsJsonPrimitive match {
            case p if p.isNumber => p.getAsNumber match {
                case n: LazilyParsedNumber => Utils.parsePrimitive(n.toString)
                case i: Int => IntElement(i.intValue())
                case l: Long => LongElement(l.longValue())
                case f: Float => FloatElement(f.floatValue())
                case d: Double => DoubleElement(d.doubleValue())
                case n => return Failure(Vector(ParseError(s"Unknown number object: $n", List())))
            }
            case p if p.isBoolean => BooleanElement(p.getAsBoolean)
            case p if p.isString => StringElement(p.getAsString)
        })
        case _ if json.isJsonArray => val array = json.getAsJsonArray
            var result: Result[List[Element]] = Success(List())

            for (i <- 0 until array.size) {
                val element = array.get(i)
                this.encodeElement(element).mapBoth(errors => result = result.flatMapBoth(errorList => Failure(errorList
                    .appendedAll(Utils.withPrependedPath(errors, ElementNode.Index(i)))))(
                    _ => Failure(Utils.withPrependedPath(errors, ElementNode.Index(i)))))(e =>
                    result = result.map(_.appended(e))
                )
            }

            result.map(list => ArrayElement(list))

        case _ if json.isJsonObject => val jsonObject = json.getAsJsonObject
            var result: Result[Map[String, Element]] = Success(Map())

            for ((key, element) <- jsonObject.entrySet.asScala.map(entry => (entry.getKey, entry.getValue))) {
                this.encodeElement(element).mapBoth(errors => result = result.flatMapBoth(errorList => Failure(errorList
                    .appendedAll(Utils.withPrependedPath(errors, key))))(_ => Failure(Utils.withPrependedPath(errors, key))))(
                    e => result = result.map(_ + (key -> e)))
            }

            result.map(map => ObjectElement(map))
    }

    override def decodeElement(element: Element): Result[JsonElement] = element match {
        case Null | None => Success(JsonNull.INSTANCE)
        case IntElement(value) => Success(new JsonPrimitive(value))
        case LongElement(value) => Success(new JsonPrimitive(value))
        case FloatElement(value) => Success(new JsonPrimitive(value))
        case DoubleElement(value) => Success(new JsonPrimitive(value))
        case BooleanElement(value) => Success(new JsonPrimitive(value))
        case StringElement(value) => Success(new JsonPrimitive(value))

        case ArrayElement(values) => val array = new JsonArray(values.size)
            var errors = Vector[ElementError]()

            for (i <- 0 until values.size) {
                val element = values(i)
                this.decodeElement(element).mapBoth(errorList => errors.appendedAll(Utils.withPrependedPath(
                    errorList, ElementNode.Index(i))))(e =>
                    array.add(e)
                )
            }

            if (errors.isEmpty)
                Success(array)
            else
                Failure(errors)
        case ObjectElement(map) => val json = new JsonObject()
            var errors = Vector[ElementError]()

            for ((key, element) <- map) {
                this.decodeElement(element).mapBoth(errorList => errors.appendedAll(Utils.withPrependedPath(
                    errorList, key)))(e =>
                    json.add(key, e)
                )
            }

            if (errors.isEmpty)
                Success(json)
            else
                Failure(errors)
    }

    override val lifecycle: Lifecycle = Lifecycle.Experimental
}
