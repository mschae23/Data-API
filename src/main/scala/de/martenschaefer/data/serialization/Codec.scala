package de.martenschaefer.data.serialization

import scala.collection.mutable.{ Buffer, ListBuffer }
import cats._
import cats.data._
import de.martenschaefer.data.serialization.Element._
import de.martenschaefer.data.serialization.ElementError._
import de.martenschaefer.data.serialization.codec.{ ArrayCodec, EitherCodec, OptionCodec, PrimitiveCodec, RecordCodec }
import de.martenschaefer.data.serialization.util.Either
import de.martenschaefer.data.serialization.util.Either._

type Decoded[T] = Either[Vector[ElementError], T]

trait Encoder[A, T] {
    def encode(value: A): T
}

object Encoder {
    def apply[A, T](using e: Encoder[A, T]): Encoder[A, T] = e
}

trait Decoder[A, T] {
    def decode(encoded: T): Decoded[A]
}

object Decoder {
    def apply[A, T](using d: Decoder[A, T]): Decoder[A, T] = d
}

trait Codec[T] {
    self =>

    def encodeElement(value: T): Element

    def decodeElement(element: Element): Decoded[T]

    def encode[E](value: T)(using Encoder[Element, E]): E =
        Encoder[Element, E].encode(this.encodeElement(value))

    def decode[E](encoded: E)(using Decoder[Element, E]): Decoded[T] =
        for {
            element <- Decoder[Element, E].decode(encoded)
            value <- this.decodeElement(element)
        } yield value

    def fieldOf(fieldName: String): IncompleteFieldCodec[T] =
        new IncompleteFieldCodec[T](fieldName) {
            def encodeElement(value: T): Element =
                self.encodeElement(value)

            def decodeElement(element: Element): Decoded[T] =
                self.decodeElement(element)
        }
}

object Codec {
    def apply[T](using c: Codec[T]): Codec[T] = c

    given Invariant[Codec] with
        def imap[A, B](codec: Codec[A])(to: A => B)(from: B => A): Codec[B] =
            new Codec[B] {
                def encodeElement(value: B): Element =
                    codec.encodeElement(from(value))

                def decodeElement(element: Element): Decoded[B] =
                    codec.decodeElement(element).map(to)
            }

    def record[T](builder: Buffer[FieldCodec[_, T]] ?=> (Buffer[FieldCodec[_, T]], ((FieldCodec[_, T] => _) ?=> T))): Codec[T] = {
        val buildTuple = builder(using ListBuffer[FieldCodec[_, T]]())
        val fields = buildTuple._1.toList
        val creator: (FieldCodec[_, T] => _) ?=> T = buildTuple._2

        new RecordCodec(fields, creator)
    }

    def build[T](builder: (FieldCodec[_, T] => _) ?=> T)(using fields: Buffer[FieldCodec[_, T]]): (Buffer[FieldCodec[_, T]], (FieldCodec[_, T] => _) ?=> T) =
        (fields, builder)

    given Codec[Int] = new PrimitiveCodec[Int, IntElement](IntElement(_),
        _.isInstanceOf[IntElement], _.value, NotAnInt(_, List()))

    given Codec[Long] = new PrimitiveCodec[Long, LongElement](LongElement(_),
        _.isInstanceOf[LongElement], _.value, NotALong(_, List()))

    given Codec[Float] = new PrimitiveCodec[Float, FloatElement](FloatElement(_),
        _.isInstanceOf[FloatElement], _.value, NotAFloat(_, List()))

    given Codec[Double] = new PrimitiveCodec[Double, DoubleElement](DoubleElement(_),
        _.isInstanceOf[DoubleElement], _.value, NotADouble(_, List()))

    given Codec[Boolean] = new PrimitiveCodec[Boolean, BooleanElement](BooleanElement(_),
        _.isInstanceOf[BooleanElement], _.value, NotABoolean(_, List()))

    given Codec[String] = new PrimitiveCodec[String, StringElement](StringElement(_),
        _.isInstanceOf[StringElement], _.value, NotAString(_, List()))

    given[T: Codec]: Codec[Option[T]] = OptionCodec[T]

    given[L: Codec, R: Codec]: Codec[Either[L, R]] = EitherCodec[L, R]

    given[T: Codec]: Codec[List[T]] = ArrayCodec[T]
}

trait IncompleteFieldCodec[T](val fieldName: String) extends Codec[T] {
    self =>

    def forGetter[B](getter: B => T)(using fields: Buffer[FieldCodec[_, B]]): FieldCodec[T, B] = {
        val codec = new FieldCodec[T, B](this.fieldName, getter) {
            def encodeElement(value: T): Element =
                self.encodeElement(value)

            def decodeElement(element: Element): Decoded[T] =
                self.decodeElement(element)
        }

        fields.addOne(codec)
        codec
    }

    def xmap[B](to: T => B)(from: B => T): Codec[B] =
        new Codec[B] {
            def encodeElement(value: B): Element =
                Element.ObjectElement(Map(self.fieldName -> self.encodeElement(from(value))))

            def decodeElement(element: Element): Decoded[B] = element match {
                case Element.ObjectElement(map) =>
                    self.decodeElement(map.get(self.fieldName).getOrElse(return Left(Vector(MissingKey(element,
                        List(ElementNode.Name(self.fieldName))))))).mapBoth(_.map(_.withPrependedPath(self.fieldName)))(to)

                case _ => Left(Vector(ElementError.NotAnObject(element, List())))
            }
        }
}

trait FieldCodec[T, B](val fieldName: String, val getter: B => T) extends Codec[T] {
    def apply(using context: FieldCodec[_, B] => _): T = context(this).asInstanceOf[T]

    def get(using context: FieldCodec[_, B] => _): T = apply
}
