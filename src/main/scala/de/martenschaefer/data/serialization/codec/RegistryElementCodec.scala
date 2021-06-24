package de.martenschaefer.data.serialization.codec

import cats.syntax.all._
import cats.effect.Sync
import de.martenschaefer.data.registry.Registry
import de.martenschaefer.data.serialization.{ Codec, Element, Result }
import de.martenschaefer.data.util.DataResult._
import de.martenschaefer.data.util.Lifecycle

class RegistryElementCodec[T](val registry: Registry[T], val elementCodec: Codec[T]) extends Codec[T] {
    override def encodeElement(value: T): Result[Element] = registry.encodeElement(value) match {
        case Failure(_, _) => elementCodec.encodeElement(value)

        case result => result
    }

    override def encodeElementIO[F[_]: Sync](value: T): F[Result[Element]] = for {
        registryEncoded <- registry.encodeElementIO(value)
        result <- registryEncoded match {
            case Failure(_, _) => elementCodec.encodeElementIO(value)

            case result => Sync[F].pure(result)
        }
    } yield result

    override def decodeElement(element: Element): Result[T] = registry.decodeElement(element) match {
        case Failure(_, _) => elementCodec.decodeElement(element)

        case result => result
    }

    override def decodeElementIO[F[_]: Sync](element: Element): F[Result[T]] = for {
        registryDecoded <- registry.decodeElementIO(element)
        result <- registryDecoded match {
            case Failure(_, _) => elementCodec.decodeElementIO(element)

            case result => Sync[F].pure(result)
        }
    } yield result

    override val lifecycle: Lifecycle = registry.lifecycle + elementCodec.lifecycle
}
