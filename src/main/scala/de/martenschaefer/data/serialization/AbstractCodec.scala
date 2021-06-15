package de.martenschaefer.data.serialization

trait AbstractCodec[A, B, R[_]] {
    def encodeValue(value: A): R[B]

    def decodeValue(encoded: B): R[A]
}
