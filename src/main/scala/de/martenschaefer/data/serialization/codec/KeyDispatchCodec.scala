package de.martenschaefer.data.serialization.codec

import scala.collection.immutable.ListMap
import de.martenschaefer.data.serialization.Element._
import de.martenschaefer.data.serialization.{ Codec, Element, ElementError, ElementNode, RecordParseError, Result }
import de.martenschaefer.data.util.DataResult._
import de.martenschaefer.data.util.{ DataResult, Lifecycle }

class KeyDispatchCodec[K: Codec, V](val typeKey: String = "type",
                                    val valueKey: String = "value",
                                    val typeFunction: V => Result[_ <: K],
                                    val codec: K => Codec[_ <: V],
                                    override val lifecycle: Lifecycle) extends Codec[V] {
    override def encodeElement(value: V): Result[Element] =
        KeyDispatchCodec.getEncoder(this.typeFunction, this.codec, value) match {
            case Success(encoder, l) =>
                Success(encoder.encodeElement(value) match {
                    case Success(ObjectElement(map), l2) => (l + l2, map.updated(this.typeKey, this.encodeType(value) match {
                        case Success(key, _) => key
                        case Failure(errors, l3) => return Failure(errors, this.lifecycle + l + l2 + l3)
                    }))
                    case Success(element, l2) => (l + l2, ListMap(this.valueKey -> element, this.typeKey -> (this.encodeType(value) match {
                        case Success(key, _) => key
                        case Failure(errors, l3) => return Failure(errors, this.lifecycle + l + l2 + l3)
                    })))
                    case Failure(errors, l) => return Failure(errors, this.lifecycle + l)
                }).flatMap(tuple => Success(ObjectElement(tuple._2), this.lifecycle + tuple._1))
            case Failure(errors, l) => Failure(errors, this.lifecycle + l)
        }

    private def encodeType(value: V): Result[Element] = Codec[K].encodeElement(this.typeFunction(value) match {
        case Success(key, _) => key
        case Failure(errors, l) => return Failure(errors, this.lifecycle + l)
    })

    override def decodeElement(element: Element): Result[V] = {
        val decodedKey = element match {
            case ObjectElement(map) => map.get(this.typeKey).map(Success(_, this.lifecycle)).getOrElse(
                Failure(List(RecordParseError.MissingKey(element, List(ElementNode.Name(this.typeKey)))), this.lifecycle))
            case _ => Failure(List(RecordParseError.NotAnObject(element, List.empty)), this.lifecycle)
        }

        decodedKey.flatMap(keyElement => Codec[K].decodeElement(keyElement).mapLeft(_.map(_.withPrependedPath(this.typeKey))))
            .flatMap(key => this.codec(key).decodeElement(element) match {
                case Failure(errors, l) => this.codec(key).decodeElement(element match {
                    case ObjectElement(map) => map.get(this.valueKey).getOrElse(return Failure(errors, this.lifecycle + l))
                    case _ => return Failure(List(RecordParseError.NotAnObject(element, List.empty)), this.lifecycle + l)
                }).addLifecycle(this.lifecycle).mapLeft(_.map(_.withPrependedPath(this.valueKey)))

                case result => result.addLifecycle(this.lifecycle)
            })
    }
}

object KeyDispatchCodec {
    private def getEncoder[K, V](typeFunction: V => Result[_ <: K], codec: K => Codec[_ <: V], input: V): Result[Codec[V]] =
        typeFunction(input).map(k => codec(k)).map(_.asInstanceOf[Codec[V]])
}
