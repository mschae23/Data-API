package de.martenschaefer.data.serialization.codec

import scala.collection.immutable.ListMap
import de.martenschaefer.data.serialization.Element._
import de.martenschaefer.data.serialization.ElementError._
import de.martenschaefer.data.serialization.{ Codec, Element, ElementError, ElementNode, FieldCodec, Result }
import de.martenschaefer.data.util.Either._
import de.martenschaefer.data.util.Lifecycle

class RecordCodec[T](fields: List[FieldCodec[_, T]], creator: (FieldCodec[_, T] => _) ?=> T) extends Codec[T] {
    def encodeElement(value: T): Result[Element] =
        Right(fields.map(field => (field.fieldName, field.encodeElement(field.getter(value))))).flatMap(fields =>
            fields.foldLeft(Vector[ElementError]())((list, fieldTuple) => fieldTuple._2 match {
                case Left(errors) => list.appendedAll(errors.map(_.withPrependedPath(fieldTuple._1)))
                case Right(result) => list
            }) match {
                case list if !list.isEmpty => Left(list)
                case _ => Right(fields.map((fieldName, result) => (fieldName, result.getRight)))
            }
        ).map(fields => ObjectElement(ListMap.from(fields)))

    def decodeElement(element: Element): Result[T] =
        element match {
            case Element.ObjectElement(map) => {
                var fieldMap = Map[FieldCodec[_, T], Any]()
                var errors = Vector[ElementError]()

                for (fieldCodec <- fields) {
                    val field = map.get(fieldCodec.fieldName).getOrElse(Element.None)
                    val decoded = fieldCodec.decodeElement(field) match {
                        case Right(value) => Right(value)
                        case Left(_) if field == Element.None => Left(Vector(MissingKey(element, List())))
                        case decoded => decoded
                    }

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
