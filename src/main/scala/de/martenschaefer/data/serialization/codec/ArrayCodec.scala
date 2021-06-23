package de.martenschaefer.data.serialization.codec

import cats.syntax.traverse.toTraverseOps
import de.martenschaefer.data.serialization.Element._
import de.martenschaefer.data.serialization.{ Codec, Element, ElementError, ElementNode, RecordParseError, Result }
import de.martenschaefer.data.util.DataResult._
import de.martenschaefer.data.util.Lifecycle

class ArrayCodec[T: Codec] extends Codec[List[T]] {
    override def encodeElement(values: List[T]): Result[Element] = {
        val encodedValues = values.map(Codec[T].encodeElement(_))

        var errors = Vector[ElementError]()
        var lifecycle = Lifecycle.Stable

        for (i <- 0 until encodedValues.size) encodedValues(i) match {
            case Failure(errorList, l) => errors = errors.appendedAll(errorList.map(_
                .withPrependedPath(ElementNode.Index(i))))
                lifecycle += l
            case Success(_, l) => lifecycle += l
        }

        if (errors.isEmpty)
            Success(ArrayElement(encodedValues.map(_.getRight)), lifecycle)
        else
            Failure(errors, lifecycle)
    }

    override def decodeElement(element: Element): Result[List[T]] = element match {
        case ArrayElement(elements) =>
            val values = elements.map(Codec[T].decodeElement(_))

            var errors = Vector[ElementError]()
            var lifecycle = Lifecycle.Stable

            for (i <- 0 until values.size) values(i) match {
                case Failure(errorList, l) => errors = errors.appendedAll(errorList.map(_
                    .withPrependedPath(ElementNode.Index(i))))
                    lifecycle += l
                case Success(_, l) => lifecycle += l
            }

            if (errors.isEmpty)
                Success(values.map(_.getRight), lifecycle)
            else
                Failure(errors, lifecycle)

        case _ => Failure(Vector(RecordParseError.NotAnArray(element, List())), this.lifecycle)
    }

    override val lifecycle: Lifecycle = Codec[T].lifecycle
}
