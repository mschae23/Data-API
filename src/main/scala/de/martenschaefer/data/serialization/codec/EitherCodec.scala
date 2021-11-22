package de.martenschaefer.data.serialization.codec

import de.martenschaefer.data.Result
import de.martenschaefer.data.serialization.Element.*
import de.martenschaefer.data.serialization.{ AlternativeError, Codec, Element, ElementError, ElementNode, RecordParseError }
import de.martenschaefer.data.util.DataResult.*
import de.martenschaefer.data.util.{ DataResult, Lifecycle }
import cats.effect.Sync
import cats.syntax.all.*

class EitherCodec[L: Codec, R: Codec] extends Codec[Either[L, R]] {
    override def encodeElement(option: Either[L, R]): Result[Element] = option match {
        case Left(value) => Codec[L].encodeElement(value)
        case Right(value) => Codec[R].encodeElement(value)
    }

    override def encodeElementIO[F[_] : Sync](option: Either[L, R]): F[Result[Element]] = option match {
        case Left(value) => Codec[L].encodeElementIO(value)
        case Right(value) => Codec[R].encodeElementIO(value)
    }

    override def decodeElement(element: Element): Result[Either[L, R]] = Codec[L].decodeElement(element) match {
        case Success(value, l) => Success(Left(value), l)
        case Failure(errors, l) => Codec[R].decodeElement(element) match {
            case Success(value, l2) => Success(Right(value), l2)
            case Failure(errors2, l2) => Failure(
                List(AlternativeError.of(List(errors, errors2), List.empty)), l + l2)
        }
    }

    override def decodeElementIO[F[_] : Sync](element: Element): F[Result[Either[L, R]]] = for {
        resultL <- Codec[L].decodeElementIO(element)
        result <- resultL match {
            case Success(value, l) => Sync[F].pure(Success(Left(value), l))
            case Failure(errors, l) => Codec[R].decodeElementIO(element).map {
                case Success(value, l2) => Success(Right(value), l2)
                case Failure(errors2, l2) => Failure(
                    List(AlternativeError.of(List(errors, errors2), List.empty)), l + l2)
            }
        }
    } yield result

    override val lifecycle: Lifecycle = Codec[L].lifecycle + Codec[R].lifecycle
}
