package de.martenschaefer.data.serialization.codec

import de.martenschaefer.data.serialization.Element._
import de.martenschaefer.data.util.Either._
import de.martenschaefer.data.serialization.{ Codec, Decoded, Element, ElementError, ElementNode }
import de.martenschaefer.data.util.Lifecycle

class ArrayCodec[T: Codec] extends Codec[List[T]] {
    override def encodeElement(values: List[T]): Element =
        ArrayElement(values.map(Codec[T].encodeElement(_)))

    override def decodeElement(element: Element): Decoded[List[T]] = element match {
        case ArrayElement(elements) =>
            val values = elements.map(Codec[T].decodeElement(_))

            var errors = Vector[ElementError]()

            for(i <- 0 until values.size) values(i) match {
                case Left(errorList) => errors = errors.appendedAll(errorList.map(_
                  .withPrependedPath(ElementNode.Index(i))))
                case _ =>
            }

            if (errors.isEmpty)
                Right(values.map(_.getRight))
            else
                Left(errors)

        case _ => Left(Vector(ElementError.NotAnArray(element, List())))
    }

    override val lifecycle: Lifecycle = Codec[T].lifecycle
}
