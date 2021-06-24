package de.martenschaefer.data.serialization.codec

import cats.syntax.all._
import cats.effect.Sync
import de.martenschaefer.data.serialization.Element._
import de.martenschaefer.data.serialization.{ Codec, Element, ElementError, ElementNode, RecordParseError, Result }
import de.martenschaefer.data.util.DataResult._
import de.martenschaefer.data.util.{ DataResult, Lifecycle }

class EitherCodec[L: Codec, R: Codec](val errorMessage: String => String) extends Codec[Either[L, R]] {
    override def encodeElement(option: Either[L, R]): Result[Element] = option match {
        case Left(value) => Codec[L].encodeElement(value)
        case Right(value) => Codec[R].encodeElement(value)
    }

    override def encodeElementIO[F[_]: Sync](option: Either[L, R]): F[Result[Element]] = option match {
        case Left(value) => Codec[L].encodeElementIO(value)
        case Right(value) => Codec[R].encodeElementIO(value)
    }

    override def decodeElement(element: Element): Result[Either[L, R]] =
        Codec[L].decodeElement(element) match {
            case Success(value, l) => Success(Left(value), l)
            case Failure(errors, l) => Codec[R].decodeElement(element) match {
                case Success(value, l2) => Success(Right(value), l + l2)
                case Failure(errors2, l2) => Failure(
                    Vector(RecordParseError.EitherParseError(this.errorMessage, element, List())), l + l2)
            }
        }

    override def decodeElementIO[F[_]: Sync](element: Element): F[Result[Either[L, R]]] = for {
        resultL <- Codec[L].decodeElementIO(element)
        result <- resultL match {
            case Success(value, l) => Sync[F].pure(Success(Left(value), l))
            case Failure(errors, l) => Codec[R].decodeElementIO(element).map(_ match {
                case Success(value, l2) => Success(Right(value), l + l2)
                case Failure(errors2, l2) => Failure(
                    Vector(RecordParseError.EitherParseError(this.errorMessage, element, List())), l + l2)
            })
        }
    } yield result

    override val lifecycle: Lifecycle = Codec[L].lifecycle + Codec[R].lifecycle
}
