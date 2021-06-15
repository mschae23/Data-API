package de.martenschaefer.data.serialization.codec

import scala.collection.immutable.ListMap
import de.martenschaefer.data.serialization.Element._
import de.martenschaefer.data.serialization.{ Codec, Element, ElementError, ElementNode, Result }
import de.martenschaefer.data.util.Either._
import de.martenschaefer.data.util.{ Either, Lifecycle }

class KeyDispatchCodec[K: Codec, V](val typeKey: String = "type",
                                    val valueKey: String = "value",
                                    val typeFunction: V => Result[_ <: K],
                                    val codec: K => Codec[_ <: V],
                                    override val lifecycle: Lifecycle) extends Codec[V] {
    override def encodeElement(value: V): Result[Element] =
        KeyDispatchCodec.getEncoder(this.typeFunction, this.codec, value) match {
            case Right(encoder) =>
                Right(ObjectElement(encoder.encodeElement(value) match {
                    case Right(ObjectElement(map)) => map.updated(this.typeKey, this.encodeType(value) match {
                        case Right(key) => key
                        case result => return result
                    })
                    case Right(element) => ListMap(this.valueKey -> element, this.typeKey -> (this.encodeType(value) match {
                        case Right(key) => key
                        case result => return result
                    }))
                    case result => return result
                }))
            case Left(errors) => Left(errors)
        }

    private def encodeType(value: V): Result[Element] = Codec[K].encodeElement(this.typeFunction(value) match {
        case Right(key) => key
        case Left(errors) => return Left(errors)
    })

    override def decodeElement(element: Element): Result[V] = {
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
    private def getEncoder[K, V](typeFunction: V => Result[_ <: K], codec: K => Codec[_ <: V], input: V): Result[Codec[V]] =
        typeFunction(input).map(k => codec(k)).map(_.asInstanceOf[Codec[V]])
}
