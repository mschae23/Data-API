package de.martenschaefer.data.serialization.codec

import scala.collection.immutable.ListMap
import de.martenschaefer.data.serialization.Element._
import de.martenschaefer.data.serialization.ElementError._
import de.martenschaefer.data.serialization.{ Codec, Element, ElementError, ElementNode, FieldCodec, Result }
import de.martenschaefer.data.util.Either._
import de.martenschaefer.data.util.{ Either, Lifecycle }

class UnitCodec[T](val value: Either[T, () => T], val lifecycle: Lifecycle = Lifecycle.Stable) extends Codec[T] {
    override def encodeElement(value: T): Result[Element] = Right(ObjectElement(ListMap()))

    override def decodeElement(element: Element): Result[T] = Right(this.value.get(v => v)(v => v()))
}
