package de.martenschaefer.data.serialization.codec

import de.martenschaefer.data.Result
import de.martenschaefer.data.serialization.Element.*
import de.martenschaefer.data.serialization.{ Codec, Element, ElementError, ElementNode }
import de.martenschaefer.data.util.DataResult.*
import de.martenschaefer.data.util.Lifecycle

class PrimitiveCodec[T, E <: Element](element: T => Element, decode: Element => Result[T],
                                      override val lifecycle: Lifecycle) extends Codec[T] {
    def this(element: T => Element, decode: Element => Result[T]) =
        this(element, decode, Lifecycle.Stable)

    override def encodeElement(value: T): Result[Element] =
        Success(this.element(value), this.lifecycle)

    override def decodeElement(element: Element): Result[T] = this.decode(element)
}