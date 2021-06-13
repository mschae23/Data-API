package de.martenschaefer.data.serialization.codec

import de.martenschaefer.data.serialization.Element._
import de.martenschaefer.data.util.Either._
import de.martenschaefer.data.serialization.{ Codec, Decoded, Element, ElementError, ElementNode }
import de.martenschaefer.data.util.{ Either, Lifecycle }

class KeyDispatchCodec[K: Codec, V](val typeKey: String = "type",
                                    val valueKey: String = "value",
                                    val typeFunction: V => Decoded[_ <: K],
                                    val codec: K => Codec[_ <: V],
                                    override val lifecycle: Lifecycle) extends Codec[V] {
    override def encodeElement(value: V): Element =
        KeyDispatchCodec.getEncoder(this.typeFunction, this.codec, value) match {
            case Right(encoder) =>
                ObjectElement(encoder.encodeElement(value) match {
                    case ObjectElement(map) => map.updated(this.typeKey, this.encodeType(value))
                    case element => Map(this.valueKey -> element, this.typeKey -> this.encodeType(value))
                })
            case Left(errors) => throw new RuntimeException(errors.toString)
        }

    private def encodeType(value: V): Element = Codec[K].encodeElement(this.typeFunction(value) match {
        case Right(key) => key
        case Left(errors) => throw new RuntimeException(errors.toString)
    })

    override def decodeElement(element: Element): Decoded[V] = {
        val decodedKey = element match {
            case ObjectElement(map) => map.get(this.typeKey).map(Right(_)).getOrElse(
                Left(Vector(ElementError.MissingKey(element, List(ElementNode.Name(this.typeKey))))))
            case _ => Left(Vector(ElementError.NotAnObject(element, List())))
        }

        decodedKey.flatMap(keyElement => Codec[K].decodeElement(keyElement).mapLeft(_.map(_.withPrependedPath(this.typeKey))))
          .flatMap(key => this.codec(key).decodeElement(element).flatOrElse(this.codec(key).decodeElement(element match {
              case ObjectElement(map) => map.get(this.valueKey).getOrElse(
                  return Left(Vector(ElementError.MissingKey(element, List(ElementNode.Name(this.valueKey))))))
              case _ => return Left(Vector(ElementError.NotAnObject(element, List())))
          }).mapLeft(_.map(_.withPrependedPath(this.valueKey)))))
    }
}

object KeyDispatchCodec {
    private def getEncoder[K, V](typeFunction: V => Decoded[_ <: K], codec: K => Codec[_ <: V], input: V): Decoded[Codec[V]] =
        typeFunction(input).map(k => codec(k)).map(_.asInstanceOf[Codec[V]])
}
