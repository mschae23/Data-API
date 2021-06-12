package de.martenschaefer.serialization.codec

import de.martenschaefer.serialization.{ Codec, Decoded, Element, ElementError, ElementNode }
import de.martenschaefer.serialization.Element._
import de.martenschaefer.serialization.util.Either
import de.martenschaefer.serialization.util.Either._

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
}
