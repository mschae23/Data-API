package de.martenschaefer.data.serialization.codec

import scala.annotation.tailrec
import cats.syntax.all._
import cats.effect.Sync
import de.martenschaefer.data.serialization.Element._
import de.martenschaefer.data.serialization.{ Codec, Element, ElementError, ElementNode, RecordParseError, Result }
import de.martenschaefer.data.util.DataResult._
import de.martenschaefer.data.util.Lifecycle

class ArrayCodec[T: Codec] extends Codec[List[T]] {
    override def encodeElement(values: List[T]): Result[Element] = {
        val encodedValues = values.map(Codec[T].encodeElement(_))

        var errors = List[ElementError]()
        var lifecycle = this.lifecycle

        for (i <- 0 until encodedValues.size) encodedValues(i) match {
            case Failure(errors2, l) => errors = errors ::: errors2.map(_
                .withPrependedPath(ElementNode.Index(i)))
                lifecycle += l
            case Success(_, l) => lifecycle += l
        }

        if (errors.isEmpty)
            Success(ArrayElement(encodedValues.map(_.getRight)), lifecycle)
        else
            Failure(errors, lifecycle)
    }

    override def encodeElementIO[F[_] : Sync](values: List[T]): F[Result[Element]] = {
        @tailrec
        def loop(errors: List[ElementError], lifecycle: Lifecycle, encodedValues: List[Result[Element]], index: Int): (List[ElementError], Lifecycle) =
            encodedValues(index) match {
                case Failure(errors2: List[ElementError], l) => (errors ::: errors2.map(_
                    .withPrependedPath(ElementNode.Index(index))), lifecycle + l)
                case Success(_, l) => (errors, lifecycle + l)
            } match {
            case (e, l) if index < encodedValues.size - 1 => loop(e, l, encodedValues, index + 1)
            case acc => acc
        }

        for {
            encodedValues <- values.map(Codec[T].encodeElementIO(_)).sequence
            acc <- Sync[F].delay(loop(List.empty, this.lifecycle, encodedValues, 0))
            errors <- Sync[F].pure(acc._1)
            lifecycle <- Sync[F].pure(acc._2)
            result <- Sync[F].delay(
                if (errors.isEmpty) Success(ArrayElement(encodedValues.map(_.getRight)), lifecycle)
                else Failure(errors, lifecycle)
            )
        } yield result
    }

    override def decodeElement(element: Element): Result[List[T]] = element match {
        case ArrayElement(elements) =>
            val values = elements.map(Codec[T].decodeElement(_))

            var errors = List[ElementError]()
            var lifecycle = Lifecycle.Stable

            for (i <- 0 until values.size) values(i) match {
                case Failure(errors2, l) => errors = errors ::: errors2.map(_
                    .withPrependedPath(ElementNode.Index(i)))
                    lifecycle += l
                case Success(_, l) => lifecycle += l
            }

            if (errors.isEmpty)
                Success(values.map(_.getRight), lifecycle)
            else
                Failure(errors, lifecycle)

        case _ => Failure(List(RecordParseError.NotAnArray(element, List.empty)), this.lifecycle)
    }

    override def decodeElementIO[F[_] : Sync](element: Element): F[Result[List[T]]] = {
        @tailrec
        def loop(errors: List[ElementError], lifecycle: Lifecycle, values: List[Result[T]], index: Int): (List[ElementError], Lifecycle) =
            values(index) match {
                case Failure(errors2, l) => (errors ::: errors2.map(_
                    .withPrependedPath(ElementNode.Index(index))), lifecycle + l)
                case Success(_, l) => (errors, lifecycle + l)
            } match {
            case (e, l) if index < values.size - 1 => loop(e, l, values, index + 1)
            case acc => acc
        }

        element match {
            case ArrayElement(elements) => for {
                values <- elements.map(Codec[T].decodeElementIO(_)).sequence
                acc <- Sync[F].delay(loop(List.empty, this.lifecycle, values, 0))
                errors <- Sync[F].pure(acc._1)
                lifecycle <- Sync[F].pure(acc._2)
                result <- Sync[F].delay(
                    if (errors.isEmpty) Success(values.map(_.getRight), lifecycle)
                    else Failure(errors, lifecycle)
                )
            } yield result
            case _ => Sync[F].pure(Failure(List(RecordParseError.NotAnArray(element, List.empty)), this.lifecycle))
        }
    }

    override val lifecycle: Lifecycle = Codec[T].lifecycle
}
