package de.martenschaefer.data.serialization.codec

import de.martenschaefer.data.Result
import de.martenschaefer.data.serialization.AlternativeError.AlternativeSubError
import de.martenschaefer.data.serialization.{ AlternativeError, Codec, Element, ElementError }
import de.martenschaefer.data.util.DataResult.*
import de.martenschaefer.data.util.Lifecycle
import cats.effect.Sync
import cats.syntax.all.*

class AlternativeCodec[T](val codecs: List[(String, Codec[T])]) extends Codec[T] {
    override def encodeElement(value: T): Result[Element] = {
        var errors: List[AlternativeSubError] = List.empty
        var lifecycle = Lifecycle.Stable

        for ((label, codec) <- this.codecs) {
            codec.encodeElement(value) match {
                case result@Success(_, _) => return result

                case Failure(codecErrors, l) =>
                    errors ::= AlternativeSubError(label, codecErrors)
                    lifecycle += l
            }
        }

        Failure(List(AlternativeError(errors.reverse)), lifecycle)
    }

    override def decodeElement(element: Element): Result[T] = {
        var errors: List[AlternativeSubError] = List.empty
        var lifecycle = Lifecycle.Stable

        for ((label, codec) <- this.codecs) {
            codec.decodeElement(element) match {
                case result@Success(_, _) => return result

                case Failure(codecErrors, l) =>
                    errors ::= AlternativeSubError(label, codecErrors)
                    lifecycle += l
            }
        }

        Failure(List(AlternativeError(errors.reverse)), lifecycle)
    }

    override val lifecycle: Lifecycle =
        this.codecs.map(_._2).foldLeft(Lifecycle.Stable)((lifecycle, codec) => lifecycle + codec.lifecycle)
}
