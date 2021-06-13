package de.martenschaefer.data.serialization.codec

import scala.collection.immutable.ListMap
import de.martenschaefer.data.serialization.Element._
import de.martenschaefer.data.serialization.ElementError._
import de.martenschaefer.data.util.Either._
import de.martenschaefer.data.serialization.{ Codec, Decoded, Element, ElementError, ElementNode, FieldCodec }
import de.martenschaefer.data.util.Lifecycle

class RecordCodec[T](fields: List[FieldCodec[_, T]], creator: (FieldCodec[_, T] => _) ?=> T) extends Codec[T] {
    def encodeElement(value: T): Element =
        Element.ObjectElement(ListMap.from(fields.map(field => (field.fieldName, field.encodeElement(field.getter(value))))))

    def decodeElement(element: Element): Decoded[T] =
        element match {
            case Element.ObjectElement(map) => {
                var fieldMap = Map[FieldCodec[_, T], Any]()
                var errors = Vector[ElementError]()

                for (fieldCodec <- fields) {
                    val field = map.get(fieldCodec.fieldName)
                    val decoded = field.map(f => fieldCodec.decodeElement(f)).getOrElse(
                        Left(Vector(MissingKey(element, List()))))

                    decoded match {
                        case Right(value) => fieldMap = fieldMap.updated(fieldCodec, value)
                        case Left(fieldErrors) => errors = errors.appendedAll(fieldErrors.map(_
                          .withPrependedPath(fieldCodec.fieldName)))
                    }
                }

                if (errors.isEmpty)
                    Right(creator(using fieldCodec => fieldMap(fieldCodec)))
                else
                    Left(errors)
            }

            case _ => Left(Vector(ElementError.NotAnObject(element, List())))
        }

    override val lifecycle: Lifecycle =
        this.fields.foldLeft(Lifecycle.Stable)((lifecycle, field) => lifecycle + field.lifecycle)
}
