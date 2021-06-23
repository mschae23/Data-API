package de.martenschaefer.data.serialization.codec

import de.martenschaefer.data.registry.Registry
import de.martenschaefer.data.serialization.{ Codec, Element, Result }
import de.martenschaefer.data.util.DataResult._
import de.martenschaefer.data.util.Lifecycle

class RegistryElementCodec[T](val registry: Registry[T], val elementCodec: Codec[T]) extends Codec[T] {
    override def encodeElement(value: T): Result[Element] = registry.encodeElement(value) match {
        case Failure(_, _) => elementCodec.encodeElement(value)

        case result => result
    }

    override def decodeElement(element: Element): Result[T] = registry.decodeElement(element) match {
        case Failure(_, _) => elementCodec.decodeElement(element)

        case result => result
    }

    override val lifecycle: Lifecycle = registry.lifecycle + elementCodec.lifecycle
}
