package de.martenschaefer.data.serialization.codec

import scala.collection.immutable.ListMap
import de.martenschaefer.data.Result
import de.martenschaefer.data.serialization.Element._
import de.martenschaefer.data.serialization.ElementError._
import de.martenschaefer.data.serialization.{ Codec, Element, ElementError, ElementNode, FieldCodec }
import de.martenschaefer.data.util.DataResult._
import de.martenschaefer.data.util.{ DataResult, Lifecycle }

class UnitCodec[T](val value: Either[T, () => T], val lifecycle: Lifecycle = Lifecycle.Stable) extends Codec[T] {
    override def encodeElement(value: T): Result[Element] = Success(ObjectElement(ListMap.empty), this.lifecycle)

    override def decodeElement(element: Element): Result[T] = Success(this.value.fold(v => v, v => v()), this.lifecycle)
}
