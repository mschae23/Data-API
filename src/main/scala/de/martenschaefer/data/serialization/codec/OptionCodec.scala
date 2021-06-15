package de.martenschaefer.data.serialization.codec

import de.martenschaefer.data.serialization.Element._
import de.martenschaefer.data.serialization.{ Codec, Element, ElementError, ElementNode, Result }
import de.martenschaefer.data.util.Either._
import de.martenschaefer.data.util.Lifecycle

class OptionCodec[T: Codec] extends Codec[Option[T]] {
    override def encodeElement(option: Option[T]): Result[Element] = option match {
        case Some(value) => Codec[T].encodeElement(value)
        case scala.None => Right(Element.None)
    }

    override def decodeElement(element: Element): Result[Option[T]] =
        element match {
            case Element.None => Right(scala.None)

            case _ => Codec[T].decodeElement(element).map(Some(_))
        }

    override val lifecycle: Lifecycle = Codec[T].lifecycle
}
