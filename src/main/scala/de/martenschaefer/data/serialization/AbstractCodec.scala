package de.martenschaefer.data.serialization

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
}