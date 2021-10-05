package de.martenschaefer.data.serialization.codec

import de.martenschaefer.data.Result
import de.martenschaefer.data.serialization.{ AlternativeError, Codec, Element, ElementError }
import de.martenschaefer.data.util.DataResult.*
import de.martenschaefer.data.util.Lifecycle
import cats.effect.Sync
import cats.syntax.all.*

class AlternativeCodec[T](val codecs: List[Codec[T]]) extends Codec[T] {
    override def encodeElement(value: T): Result[Element] = {
        var errors: List[List[ElementError]] = List.empty
        var lifecycle = Lifecycle.Stable

        for (codec <- this.codecs) {
            codec.encodeElement(value) match {
                case result@Success(_, _) => return result

                case Failure(codecErrors, l) => {
                    errors ::= codecErrors
                    lifecycle += l
                }
            }
        }

        Failure(List(AlternativeError(errors.reverse, List.empty)), lifecycle)
    }

    override def encodeElementIO[F[_] : Sync](value: T): F[Result[Element]] = for {
        encoded <- this.codecs.map(_.encodeElementIO(value)).sequence
        errors <- Sync[F].delay(encoded.foldLeft((List.empty[List[ElementError]], Lifecycle.Stable))((acc, encodedElement) =>
            encodedElement match {
                case result @ Success(_, _) => return Sync[F].pure(result)

                case Failure(errors, l) => (errors :: acc._1, acc._2 + l)
            }))
    } yield Failure(List(AlternativeError(errors._1.reverse, List.empty)), lifecycle)

    override def decodeElement(element: Element): Result[T] = {
        var errors: List[List[ElementError]] = List.empty
        var lifecycle = Lifecycle.Stable

        for (codec <- this.codecs) {
            codec.decodeElement(element) match {
                case result@Success(_, _) => return result

                case Failure(codecErrors, l) => {
                    errors ::= codecErrors
                    lifecycle += l
                }
            }
        }

        Failure(List(AlternativeError(errors.reverse, List.empty)), lifecycle)
    }

    override def decodeElementIO[F[_]: Sync](element: Element): F[Result[T]] = for {
        decoded <- this.codecs.map(_.decodeElementIO(element)).sequence
        errors <- Sync[F].delay(decoded.foldLeft((List.empty[List[ElementError]], Lifecycle.Stable))((acc, decodedElement) =>
            decodedElement match {
                case result @ Success(_, _) => return Sync[F].pure(result)

                case Failure(errors, l) => (errors :: acc._1, acc._2 + l)
            }))
    } yield Failure(List(AlternativeError(errors._1.reverse, List.empty)), lifecycle)

    override val lifecycle: Lifecycle =
        this.codecs.foldLeft(Lifecycle.Stable)((lifecycle, codec) => lifecycle + codec.lifecycle)
}
