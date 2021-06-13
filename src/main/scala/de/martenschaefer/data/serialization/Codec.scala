package de.martenschaefer.data.serialization

import scala.collection.mutable.{ Buffer, ListBuffer }
import cats._
import cats.data._
import de.martenschaefer.data.serialization.Element._
import de.martenschaefer.data.serialization.ElementError._
import de.martenschaefer.data.serialization.codec.{ ArrayCodec, EitherCodec, KeyDispatchCodec, OptionCodec, PrimitiveCodec, RecordCodec }
import de.martenschaefer.data.util.Either._
import de.martenschaefer.data.util.{ Either, Lifecycle }

type Decoded[T] = Either[Vector[ElementError], T]

trait Encoder[A, B] {
    def encode(value: A): B
}

object Encoder {
    def apply[A, B](using e: Encoder[A, B]): Encoder[A, B] = e
}

trait Decoder[A, B] {
    def decode(encoded: B): Decoded[A]
}

object Decoder {
    def apply[A, B](using d: Decoder[A, B]): Decoder[A, B] = d
}

trait Codec[T] {
    self =>

    def encodeElement(value: T): Element

    def decodeElement(element: Element): Decoded[T]

    val lifecycle: Lifecycle

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

            override val lifecycle: Lifecycle = self.lifecycle
        }

    def xmap[B](to: T => B)(from: B => T): Codec[B] = Invariant[Codec].imap(this)(to)(from)

    def dispatch[B](typeKey: String, typeFunction: B => T, codec: T => Codec[_ <: B], lifecycle: Lifecycle): Codec[B] =
        new KeyDispatchCodec[T, B](typeKey, b => Right(typeFunction(b)), codec, lifecycle)(using this)

    def dispatch[B](typeKey: String, typeFunction: B => T, codec: T => Codec[_ <: B]): Codec[B] =
        this.dispatch(typeKey, typeFunction, codec, this.lifecycle)

    def dispatchStable[B](typeKey: String, typeFunction: B => T, codec: T => Codec[_ <: B]): Codec[B] =
        this.dispatch(typeKey, typeFunction, codec, Lifecycle.Stable)

    def dispatch[B](typeFunction: B => T, codec: T => Codec[_ <: B]): Codec[B] =
        this.dispatch("type", typeFunction, codec)

    def dispatchStable[B](typeFunction: B => T, codec: T => Codec[_ <: B]): Codec[B] =
        this.dispatch("type", typeFunction, codec, Lifecycle.Stable)

    def withLifecycle(newLifecycle: Lifecycle): Codec[T] = new Codec[T] {
        def encodeElement(value: T): Element = self.encodeElement(value)

        def decodeElement(element: Element): Decoded[T] = self.decodeElement(element)

        val lifecycle: Lifecycle = newLifecycle
    }

    def stable: Codec[T] = this.withLifecycle(Lifecycle.Stable)

    def experimental: Codec[T] = this.withLifecycle(Lifecycle.Experimental)

    def deprecated(since: Int): Codec[T] = this.withLifecycle(Lifecycle.Deprecated(since))
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

                override val lifecycle: Lifecycle = codec.lifecycle
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

            override val lifecycle: Lifecycle = self.lifecycle
        }

        fields.addOne(codec)
        codec
    }

    override def xmap[B](to: T => B)(from: B => T): Codec[B] =
        new Codec[B] {
            def encodeElement(value: B): Element =
                Element.ObjectElement(Map(self.fieldName -> self.encodeElement(from(value))))

            def decodeElement(element: Element): Decoded[B] = element match {
                case Element.ObjectElement(map) =>
                    self.decodeElement(map.get(self.fieldName).getOrElse(return Left(Vector(MissingKey(element,
                        List(ElementNode.Name(self.fieldName))))))).mapBoth(_.map(_.withPrependedPath(self.fieldName)))(to)

                case _ => Left(Vector(ElementError.NotAnObject(element, List())))
            }

            override val lifecycle: Lifecycle = self.lifecycle
        }
}

trait FieldCodec[T, B](val fieldName: String, val getter: B => T) extends Codec[T] {
    def apply(using context: FieldCodec[_, B] => _): T = context(this).asInstanceOf[T]

    def get(using context: FieldCodec[_, B] => _): T = apply
}
