package de.martenschaefer.data.serialization.codec

import cats.syntax.all._
import cats.effect.Sync
import de.martenschaefer.data.Result
import de.martenschaefer.data.serialization.{ Codec, Element, ElementError, ElementNode }
import de.martenschaefer.data.util.DataResult._
import de.martenschaefer.data.util.Lifecycle

class OptionCodec[T: Codec] extends Codec[Option[T]] {
    override def encodeElement(option: Option[T]): Result[Element] = option match {
        case Some(value) => Codec[T].encodeElement(value)
        case None => Success(Element.None, this.lifecycle)
    }

    override def encodeElementIO[F[_]: Sync](option: Option[T]): F[Result[Element]] = option match {
        case Some(value) => Codec[T].encodeElementIO(value)
        case None => Sync[F].pure(Success(Element.None, this.lifecycle))
    }

    override def decodeElement(element: Element): Result[Option[T]] = element match {
        case Element.None => Success(None, this.lifecycle)

        case _ => Codec[T].decodeElement(element).map(Some(_))
    }

    override def decodeElementIO[F[_]: Sync](element: Element): F[Result[Option[T]]] = element match {
        case Element.None => Sync[F].pure(Success(None, this.lifecycle))

        case _ => Codec[T].decodeElementIO(element).map(_.map(Some(_)))
    }

    override val lifecycle: Lifecycle = Codec[T].lifecycle
}
