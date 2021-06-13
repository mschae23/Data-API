package de.martenschaefer.data.serialization.codec

import de.martenschaefer.data.serialization.ElementError
import de.martenschaefer.data.serialization.{ Codec, Element, ElementError, ElementNode }
import de.martenschaefer.data.serialization.Decoded
import de.martenschaefer.data.serialization.Element._
import de.martenschaefer.data.serialization.util.Either._

class PrimitiveCodec[T, E <: Element](val element: T => Element, isElement: Element => Boolean, getter: E => T, val error: Element => ElementError) extends Codec[T] {
    override def encodeElement(value: T): Element =
        this.element(value)

    override def decodeElement(element: Element): Decoded[T] =
        if (this.isElement(element))
            Right(this.getter(element.asInstanceOf[E]))
        else
            Left(Vector(this.error(element)))
}