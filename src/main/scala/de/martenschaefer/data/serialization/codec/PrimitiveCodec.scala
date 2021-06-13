package de.martenschaefer.data.serialization.codec

import de.martenschaefer.data.serialization.ElementError
import de.martenschaefer.data.serialization.{ Codec, Element, ElementError, ElementNode }
import de.martenschaefer.data.serialization.Decoded
import de.martenschaefer.data.serialization.Element._
import de.martenschaefer.data.serialization.util.Either._
import de.martenschaefer.data.util.Lifecycle

class PrimitiveCodec[T, E <: Element](element: T => Element, isElement: Element => Boolean,
                                      getter: E => T, val error: Element => ElementError,
                                      override val lifecycle: Lifecycle) extends Codec[T] {
    def this(element: T => Element, isElement: Element => Boolean, getter: E => T, error: Element => ElementError) =
        this(element, isElement, getter, error, Lifecycle.Stable)

    override def encodeElement(value: T): Element =
        this.element(value)

    override def decodeElement(element: Element): Decoded[T] =
        if (this.isElement(element))
            Right(this.getter(element.asInstanceOf[E]))
        else
            Left(Vector(this.error(element)))
}