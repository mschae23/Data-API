package de.martenschaefer.data.serialization.codec

import cats.syntax.all._
import cats.effect.Sync
import de.martenschaefer.data.serialization.{ Codec, Element, Result }
import de.martenschaefer.data.util.Lifecycle
import de.martenschaefer.data.util.DataResult._

class AlternativeCodec[T](codec: Codec[T], alternative: Codec[T]) extends Codec[T] {
    override def encodeElement(value: T): Result[Element] = this.codec.encodeElement(value) match {
        case Failure(errors, l) => this.alternative.encodeElement(value) match {
            case Failure(_, _) => Failure(errors, l)
            case result => result
        }

        case result => result
    }

    override def encodeElementIO[F[_]: Sync](value: T): F[Result[Element]] = for {
        encoded <- this.codec.encodeElementIO(value)
        encodedWithAlternative <- encoded match {
            case Failure(errors, l) => this.alternative.encodeElementIO(value).map(_ match {
                case Failure(_, _) => Failure(errors, l)
                case result => result
            })
            case result => Sync[F].pure(result)
        }
    } yield encodedWithAlternative

    override def decodeElement(element: Element): Result[T] = this.codec.decodeElement(element) match {
        case Failure(errors, l) => this.alternative.decodeElement(element) match {
            case Failure(_, _) => Failure(errors, l)
            case result => result
        }

        case result => result
    }

    override def decodeElementIO[F[_]: Sync](element: Element): F[Result[T]] = for {
        decoded <- this.codec.decodeElementIO(element)
        decodedWithAlternative <- decoded match {
            case Failure(errors, l) => this.alternative.decodeElementIO(element).map(_ match {
                case Failure(_, _) => Failure(errors, l)
                case result => result
            })
            case result => Sync[F].pure(result)
        }
    } yield decodedWithAlternative

    override val lifecycle: Lifecycle = this.codec.lifecycle + this.alternative.lifecycle
}
