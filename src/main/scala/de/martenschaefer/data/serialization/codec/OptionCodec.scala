package de.martenschaefer.data.serialization.codec

import de.martenschaefer.data.serialization.ElementError
import de.martenschaefer.data.serialization.{ Codec, Element, ElementError, ElementNode }
import de.martenschaefer.data.serialization.Decoded
import de.martenschaefer.data.serialization.Element._
import de.martenschaefer.data.serialization.util.Either._
import de.martenschaefer.data.util.Lifecycle

class OptionCodec[T: Codec] extends Codec[Option[T]] {
    override def encodeElement(option: Option[T]): Element = option match {
        case Some(value) => Codec[T].encodeElement(value)
        case scala.None => Element.None
    }

    override def decodeElement(element: Element): Decoded[Option[T]] =
        element match {
            case Element.None => Right(scala.None)

            case _ => Codec[T].decodeElement(element).map(Some(_))
        }

    override val lifecycle: Lifecycle = Codec[T].lifecycle
}
