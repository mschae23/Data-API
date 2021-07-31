package de.martenschaefer.data.serialization

import cats.{ Functor, Invariant }
import cats.syntax.functor._
import de.martenschaefer.data.util.Lifecycle

/**
 * An {@code AbstractCodec} can convert between objects of type {@code A} and {@code B}.
 *
 * @tparam A Type of "decoded" object
 * @tparam B Type of "encoded" object
 * @tparam E Result type for encoding
 * @tparam D Result type for decoding
 */
trait AbstractCodec[A, B, E[_], D[_]] {
    def encodeValue(value: A): E[B]

    def decodeValue(encoded: B): D[A]
}

object AbstractCodec {
    def apply[A, B, E[_], D[_], C <: AbstractCodec[A, B, E, D]](using c: C) = c

    given [B, E[_], D[_]: Functor]: Invariant[[A] =>> AbstractCodec[A, B, E, D]] with
        type F[A] = AbstractCodec[A, B, E, D]

        def imap[A, C](codec: F[A])(to: A => C)(from: C => A): F[C] =
            new AbstractCodec[C, B, E, D] {
                def encodeValue(value: C): E[B] =
                    codec.encodeValue(from(value))

                def decodeValue(value: B): D[C] =
                    codec.decodeValue(value).map(to)
            }
}