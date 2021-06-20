package de.martenschaefer.data.serialization.codec

import de.martenschaefer.data.serialization.{ Codec, Element, Result }
import de.martenschaefer.data.util.Lifecycle
import de.martenschaefer.data.util.Either._

class AlternativeCodec[T](codec: Codec[T], alternative: Codec[T]) extends Codec[T] {
    override def encodeElement(value: T): Result[Element] = codec.encodeElement(value) match {
        case Left(errors) => alternative.encodeElement(value) match {
            case Left(_) => Left(errors)
            case result => result
        }

        case result => result
    }

    override def decodeElement(element: Element): Result[T] = codec.decodeElement(element) match {
        case Left(errors) => alternative.decodeElement(element) match {
            case Left(_) => Left(errors)
            case result => result
        }

        case result => result
    }

    override val lifecycle: Lifecycle = codec.lifecycle + alternative.lifecycle
}
