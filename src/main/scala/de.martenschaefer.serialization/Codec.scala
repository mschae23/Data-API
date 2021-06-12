package de.martenschaefer.serialization

import scala.collection.mutable.{ Buffer, ListBuffer }
import cats._
import cats.data._
import de.martenschaefer.serialization.util.Either
import de.martenschaefer.serialization.util.Either._

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

    def record[T](builder: Buffer[FieldCodec[_, T]] ?=> (Buffer[FieldCodec[_, T]], (Map[String, _] ?=> T))): Codec[T] = {
        val buildTuple = builder(using ListBuffer[FieldCodec[_, T]]())
        val fields = buildTuple._1.toList
        val creator: Map[String, _] ?=> T = buildTuple._2

        new Codec[T] {
            def encodeElement(value: T): Element =
                Element.ObjectElement(fields.map(field => (field.fieldName, field.encodeElement(field.getter(value)))).toMap)

            def decodeElement(element: Element): Decoded[T] =
                element match {
                    case Element.ObjectElement(map) => {
                        val valueDecodedMap = map.map((name, fieldElement) =>
                            (name, fields.find(codec => codec.fieldName == name)
                              .get.decodeElement(fieldElement)))

                        var errors = Vector[ElementError]()

                        for ((name, decoded) <- valueDecodedMap)
                            decoded match {
                                case Left(errors2) => errors = errors.appendedAll(errors2.map(_.withPrependedPath(name)))
                                case _ =>
                            }

                        if (errors.isEmpty)
                            Right(creator(using valueDecodedMap.map((name, decoded) => (name, decoded.getRight))))
                        else
                            Left(errors)
                    }

                    case _ => Left(Vector(ElementError.NotAnObject(element, List())))
                }
        }
    }

    def build[T](builder: Map[String, _] ?=> T)(using fields: Buffer[FieldCodec[_, T]]): (Buffer[FieldCodec[_, T]], Map[String, _] ?=> T) =
        (fields, builder)

    given Codec[Int] with {
        override def encodeElement(value: Int): Element =
            Element.IntElement(value)

        override def decodeElement(element: Element): Decoded[Int] =
            element match {
                case Element.IntElement(value) => Right(value)

                case _ => Left(Vector(ElementError.NotAnInt(element, List())))
            }
    }

    given Codec[Long] with {
        override def encodeElement(value: Long): Element =
            Element.LongElement(value)

        override def decodeElement(element: Element): Decoded[Long] =
            element match {
                case Element.LongElement(value) => Right(value)

                case _ => Left(Vector(ElementError.NotALong(element, List())))
            }
    }

    given Codec[Float] with{
        override def encodeElement(value: Float): Element =
            Element.FloatElement(value)

        override def decodeElement(element: Element): Decoded[Float] =
            element match {
                case Element.FloatElement(value) => Right(value)

                case _ => Left(Vector(ElementError.NotAFloat(element, List())))
            }
    }

    given Codec[Double] with {
        override def encodeElement(value: Double): Element =
            Element.DoubleElement(value)

        override def decodeElement(element: Element): Decoded[Double] =
            element match {
                case Element.DoubleElement(value) => Right(value)

                case _ => Left(Vector(ElementError.NotAFloat(element, List())))
            }
    }

    given Codec[Boolean] with {
        override def encodeElement(value: Boolean): Element =
            Element.BooleanElement(value)

        override def decodeElement(element: Element): Decoded[Boolean] =
            element match {
                case Element.BooleanElement(value) => Right(value)

                case _ => Left(Vector(ElementError.NotABoolean(element, List())))
            }
    }

    given Codec[String] with {
        override def encodeElement(value: String): Element =
            Element.StringElement(value)

        override def decodeElement(element: Element): Decoded[String] =
            element match {
                case Element.StringElement(value) => Right(value)

                case _ => Left(Vector(ElementError.NotAString(element, List())))
            }
    }

    given [T: Codec]: Codec[Option[T]] with {
        override def encodeElement(option: Option[T]): Element = option match {
            case Some(value) => Codec[T].encodeElement(value)
            case None => Element.None
        }

        override def decodeElement(element: Element): Decoded[Option[T]] =
            element match {
                case Element.None => Right(None)

                case _ => Codec[T].decodeElement(element).map(Some(_))
            }
    }

    given [L: Codec, R: Codec]: Codec[Either[L, R]] with {
        override def encodeElement(option: Either[L, R]): Element = option match {
            case Left(value) => Codec[L].encodeElement(value)
            case Right(value) => Codec[R].encodeElement(value)
        }

        override def decodeElement(element: Element): Decoded[Either[L, R]] =
            Codec[L].decodeElement(element) match {
                case Right(value) => Right(Left(value))
                case Left(errors) => Codec[R].decodeElement(element) match {
                    case Right(value) => Right(Right(value))
                    case Left(errors2) => Left(Vector(ElementError.Neither(element, List())))
                }
            }
    }
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
                    self.decodeElement(map.get(self.fieldName).get).mapBoth(_.map(_
                      .withPrependedPath(self.fieldName)))(to)

                case _ => Left(Vector(ElementError.NotAnObject(element, List())))
            }
        }
}

trait FieldCodec[T, B](val fieldName: String, val getter: B => T) extends Codec[T] {
    def apply(using map: Map[String, _]): T =
        try {
            map.get(this.fieldName).get.asInstanceOf[T]
        } catch {
            case e: (NoSuchElementException | ClassCastException) =>
                throw new NoSuchElementException(e.getMessage)
        }

    def get(using map: Map[String, _]): T = apply
}
