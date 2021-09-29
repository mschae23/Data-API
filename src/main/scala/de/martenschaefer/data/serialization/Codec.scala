package de.martenschaefer.data.serialization

import scala.collection.mutable.{ Buffer, ListBuffer }
import cats._
import cats.data._
import cats.effect.kernel._
import cats.syntax.all._
import de.martenschaefer.data.Result
import de.martenschaefer.data.registry.Registry
import de.martenschaefer.data.serialization.Element._
import de.martenschaefer.data.serialization.RecordParseError._
import de.martenschaefer.data.serialization.codec.{ AlternativeCodec, ArrayCodec, DerivedCodec, EitherCodec, KeyDispatchCodec, OptionCodec, PrimitiveCodec, RecordCodec, UnitCodec }
import de.martenschaefer.data.util.{ DataResult, Lifecycle }
import de.martenschaefer.data.util.DataResult._
import shapeless3.deriving.{ K0, Labelling }

/**
 * Can encode objects of type {@code A} to objects of type {@code B}.
 *
 * @tparam A Type of objects that can be encoded
 * @tparam B Type of the object that {@code A} will be encoded to
 */
trait Encoder[A, B] {
    def encode(value: A): Result[B]
}

object Encoder {
    def apply[A, B](using e: Encoder[A, B]): Encoder[A, B] = e
}

/**
 * Can decode objects of type {@code A} from objects of type {@code B}.
 *
 * @tparam A Type of objects that can be decoded
 * @tparam B Type of the object that {@code A} will be decoded from
 */
trait Decoder[A, B] {
    def decode(encoded: B): Result[A]
}

object Decoder {
    def apply[A, B](using d: Decoder[A, B]): Decoder[A, B] = d
}

/**
 * A {@code Codec} can encode an object to an {@link Element}, and decode {@code Element}s to objects.
 *
 * @tparam T The type of the objects that will be encoded or decoded by this
 */
trait Codec[T] extends AbstractCodec[T, Element, Result, Result] {
    self =>

    /**
     * Encodes an object of type {@code T} to an {@link Element}.
     *
     * @param value The object that will be encoded
     * @return The encoded {@code Element}, or a list of errors
     */
    def encodeElement(value: T): Result[Element]

    /**
     * Decodes an object of type {@code T} from an {@link Element}.
     *
     * @param element The {@code Element} that will be decoded
     * @return The decoded object, or a list of errors
     */
    def decodeElement(element: Element): Result[T]

    /**
     * This {@code Codec}'s lifecycle.
     * If the {@code Codec} is derived from something else that has a {@code Lifecycle},
     * it should be inherited from that.
     */
    val lifecycle: Lifecycle

    /**
     * Creates an instance of {@code F} that encodes an object of type {@code T} to an {@link Element}.
     *
     * @tparam F Return type that has to have a {@code Sync} instance.
     * @param value The object that will be encoded
     * @return Instance of {@code F} that can return the encoded {@code Element}, or a list of errors
     */
    def encodeElementIO[F[_]: Sync](value: T): F[Result[Element]] = Sync[F].delay(this.encodeElement(value))

    /**
     * Creates an instance of {@code F} that decodes an object of type {@code T} from an {@link Element}.
     *
     * @tparam F Return type that has to have a {@code Sync} instance.
     * @param element The {@code Element} that will be decoded
     * @return Instance of {@code F} that can return the decoded object, or a list of errors
     */
    def decodeElementIO[F[_]: Sync](element: Element): F[Result[T]] = Sync[F].delay(this.decodeElement(element))

    /**
     * Encodes an object of type {@code T} to another object using an {@link Encoder}.
     *
     * @param value The object that will be encoded
     * @tparam E Return type. Has to be the {@code B} type of the used {@code Encoder}.
     * @return The encoded object
     */
    def encode[E](value: T)(using Encoder[Element, E]): Result[E] =
        this.encodeElement(value).flatMap(Encoder[Element, E].encode(_))

    /**
     * Creates an instance of {@code F} that encodes an object of type {@code T} to another object using an {@link Encoder}.
     *
     * @param value The object that will be encoded
     * @tparam F Return type that has to have a {@code Sync} instance.
     * @tparam E Type of the encoded object. Has to be the {@code B} type of the used {@code Encoder}.
     * @return The instance of {@code F} that returns the encoded object
     */
    def encodeIO[F[_]: Sync, E](value: T)(using Encoder[Element, E]): F[Result[E]] =
        this.encodeElementIO(value).map(_.flatMap(Encoder[Element, E].encode(_)))

    /**
     * Decodes an object of type {@code T} from another object using a {@link Decoder}.
     *
     * @param encoded The object that will be decoded
     * @tparam E Type of the object that will be decoded. Has to be the {@code B} type of the used {@code Decoder}.
     * @return The decoded object
     */
    def decode[E](encoded: E)(using Decoder[Element, E]): Result[T] =
        for {
            element <- Decoder[Element, E].decode(encoded)
            value <- this.decodeElement(element)
        } yield value

    /**
     * Creates an instance of {@code F} that decodes an object of type {@code T} from another object using a {@link Decoder}.
     *
     * @param encoded The object that will be decoded
     * @tparam F Return type that has to have a {@code Sync} instance.
     * @tparam E Type of the object that will be decoded. Has to be the {@code B} type of the used {@code Decoder}.
     * @return The instance of {@code F} that returns the decoded object
     */
    def decodeIO[F[_]: Sync, E](encoded: E)(using Decoder[Element, E]): F[Result[T]] =
        Sync[F].delay(this.decode(encoded))

    override def encodeValue(value: T): Result[Element] = this.encodeElement(value)

    override def decodeValue(value: Element): Result[T] = this.decodeElement(value)

    /**
     * Creates an imcomplete field codec.
     *
     * @param fieldName Name of the field.
     * @return The incomplete field codec
     */
    def fieldOf(fieldName: String): IncompleteFieldCodec[T] =
        new IncompleteFieldCodec[T](fieldName) {
            def encodeElement(value: T): Result[Element] =
                self.encodeElement(value)

            def decodeElement(element: Element): Result[T] =
                self.decodeElement(element)

            override val lifecycle: Lifecycle = self.lifecycle
        }

    /**
     * Creates a {@code Codec} for a new type that uses this one.
     *
     * @param to   Function that turns an object of type {@code T} to an object of the new type.
     * @param from Function that turns an object of the new type back to an object of type {@code T}.
     * @tparam B The new type.
     * @return The created {@code Codec}
     */
    def xmap[B](to: T => B)(from: B => T): Codec[B] = Invariant[Codec].imap(this)(to)(from)

    /**
     * Creates a {@code Codec} for a new type that uses this one.
     *
     * @param to   Function that turns an object of type {@code T} to an object of the new type, or a list of errors.
     * @param from Function that turns an object of the new type back to an object of type {@code T}.
     * @tparam B The new type.
     * @return The created {@code Codec}
     */
    def flatXmap[B](to: (T, Element) => Result[B])(from: B => Result[T]): Codec[B] = new Codec[B] {
        def encodeElement(value: B): Result[Element] =
            from(value).flatMapWithLifecycle((t, l) => self.encodeElement(t).addLifecycle(l))

        override def encodeElementIO[F[_]: Sync](value: B): F[Result[Element]] = for {
            resultT <- Sync[F].delay(from(value))
            encoded <- resultT match {
                case Success(t, l) => self.encodeElementIO(t).map(_.addLifecycle(l))
                case Failure(errors, l) => Sync[F].delay(Failure(errors, l))
            }
        } yield encoded

        def decodeElement(element: Element): Result[B] =
            self.decodeElement(element).flatMapWithLifecycle((b, l) => to(b, element).addLifecycle(l))

        override def decodeElementIO[F[_]: Sync](element: Element): F[Result[B]] = for {
            elementResult <- self.decodeElementIO(element)
            resultB <- Sync[F].delay(elementResult.flatMapWithLifecycle((b, l) => to(b, element).withLifecycle(l)))
        } yield resultB

        override val lifecycle: Lifecycle = self.lifecycle
    }

    /**
     * Creates a new {@code Codec} that returns a default value if decoding fails.
     *
     * @param alternative The default value
     * @return The created {@code Codec}
     */
    def orElse(alternative: T): Codec[T] = new Codec {
        override def encodeElement(value: T): Result[Element] = self.encodeElement(value)

        override def decodeElement(element: Element): Result[T] = self.decodeElement(element) match {
            case Failure(_, l) => Success(alternative, l)
            case result => result
        }

        override def decodeElementIO[F[_]: Sync](element: Element): F[Result[T]] = for {
            selfResultT <- self.decodeElementIO(element)
            resultT <- selfResultT match {
                case Failure(_, l) => Sync[F].delay(Success(alternative, l))
                case result => Sync[F].delay(result)
            }
        } yield resultT

        override val lifecycle: Lifecycle = self.lifecycle
    }

    /**
     * Creates a new {@code Codec} that returns a default value if decoding fails.
     *
     * @param alternative The default value
     * @return The created {@code Codec}
     */
    def orElse(alternative: () => T): Codec[T] = new Codec {
        override def encodeElement(value: T): Result[Element] = self.encodeElement(value)

        override def decodeElement(element: Element): Result[T] = self.decodeElement(element) match {
            case Failure(_, l) => Success(alternative(), l)
            case result => result
        }

        override def decodeElementIO[F[_]: Sync](element: Element): F[Result[T]] = for {
            selfResultT <- self.decodeElementIO(element)
            resultT <- selfResultT match {
                case Failure(_, l) => Sync[F].delay(Success(alternative(), l))
                case result => Sync[F].delay(result)
            }
        } yield resultT

        override val lifecycle: Lifecycle = self.lifecycle
    }

    /**
     * Creates a new {@code Codec} that tries another {@code Codec} as well if encoding or decoding fails.
     *
     * @param alternative The alternative {@code Codec}
     * @return The created {@code Codec}
     */
    def flatOrElse(alternative: Codec[T]): Codec[T] = new AlternativeCodec(this, alternative)

    /**
     * Creates a {@code Codec} for objects that have a type,
     * where objects of different types can have individual {@code Codec}s themselves.
     * The {@code Codec} that this method is called on will be used to encode the type.
     *
     * This is especially useful in combination with a {@link Registry}.
     *
     * @param typeKey      Name of the field used for the type.
     * @param valueKey     Name of the field that can be used for the value
     *                     (is optional for objects, but required for primitive values).
     * @param typeFunction Function that gets the type of an object.
     * @param codec        Function that gets a {@code Codec} for the value from the type.
     * @param lifecycle    The {@link Lifecycle} of the new {@code Codec}.
     * @tparam B Type for the new {@code Codec}.
     * @return The new {@code Codec}
     */
    def dispatch[B](typeKey: String, valueKey: String, typeFunction: B => T, codec: T => Codec[_ <: B], lifecycle: Lifecycle): Codec[B] =
        new KeyDispatchCodec[T, B](typeKey, valueKey, b => Success(typeFunction(b), lifecycle), codec, lifecycle)(using this)

    /**
     * Creates a {@code Codec} for objects that have a type,
     * where objects of different types can have individual {@code Codec}s themselves.
     * The {@code Codec} that this method is called on will be used to encode the type,
     * and for the lifecycle of the new {@code Codec}.
     *
     * This is especially useful in combination with a {@link Registry}.
     *
     * @param typeKey      Name of the field used for the type.
     * @param valueKey     Name of the field that can be used for the value
     *                     (is optional for objects, but required for primitive values).
     * @param typeFunction Function that gets the type of an object.
     * @param codec        Function that gets a {@code Codec} for the value from the type.
     * @tparam B Type for the new {@code Codec}.
     * @return The new {@code Codec}
     */
    def dispatch[B](typeKey: String, valueKey: String, typeFunction: B => T, codec: T => Codec[_ <: B]): Codec[B] =
        this.dispatch(typeKey, valueKey, typeFunction, codec, this.lifecycle)

    /**
     * Creates a {@code Codec} for objects that have a type,
     * where objects of different types can have individual {@code Codec}s themselves.
     * The {@code Codec} that this method is called on will be used to encode the type.
     *
     * This is especially useful in combination with a {@link Registry}.
     *
     * The new {@code Codec}'s lifecycle will be {@code Stable}.
     *
     * @param typeKey      Name of the field used for the type.
     * @param valueKey     Name of the field that can be used for the value
     *                     (is optional for objects, but required for primitive values).
     * @param typeFunction Function that gets the type of an object.
     * @param codec        Function that gets a {@code Codec} for the value from the type.
     * @tparam B Type for the new {@code Codec}.
     * @return The new {@code Codec}
     */
    def dispatchStable[B](typeKey: String, valueKey: String, typeFunction: B => T, codec: T => Codec[_ <: B]): Codec[B] =
        this.dispatch(typeKey, valueKey, typeFunction, codec, Lifecycle.Stable).stable

    /**
     * Creates a {@code Codec} for objects that have a type,
     * where objects of different types can have individual {@code Codec}s themselves.
     * The {@code Codec} that this method is called on will be used to encode the type,
     * and for the lifecycle of the new {@code Codec}.
     *
     * This is especially useful in combination with a {@link Registry}.
     *
     * @param typeKey      Name of the field used for the type.
     * @param typeFunction Function that gets the type of an object.
     * @param codec        Function that gets a {@code Codec} for the value from the type.
     * @tparam B Type for the new {@code Codec}.
     * @return The new {@code Codec}
     */
    def dispatch[B](typeKey: String, typeFunction: B => T, codec: T => Codec[_ <: B]): Codec[B] =
        this.dispatch(typeKey, "value", typeFunction, codec, this.lifecycle)

    /**
     * Creates a {@code Codec} for objects that have a type,
     * where objects of different types can have individual {@code Codec}s themselves.
     * The {@code Codec} that this method is called on will be used to encode the type.
     *
     * This is especially useful in combination with a {@link Registry}.
     *
     * The new {@code Codec}'s lifecycle will be {@code Stable}.
     *
     * @param typeKey Name of the field used for the type.
     * @param codec   Function that gets a {@code Codec} for the value from the type.
     * @tparam B Type for the new {@code Codec}.
     * @return The new {@code Codec}
     */
    def dispatchStable[B](typeKey: String, typeFunction: B => T, codec: T => Codec[_ <: B]): Codec[B] =
        this.dispatch(typeKey, "value", typeFunction, codec, Lifecycle.Stable).stable

    /**
     * Creates a {@code Codec} for objects that have a type,
     * where objects of different types can have individual {@code Codec}s themselves.
     * The {@code Codec} that this method is called on will be used to encode the type,
     * and for the lifecycle of the new {@code Codec}.
     *
     * This is especially useful in combination with a {@link Registry}.
     *
     * @param typeFunction Function that gets the type of an object.
     * @param codec        Function that gets a {@code Codec} for the value from the type.
     * @tparam B Type for the new {@code Codec}.
     * @return The new {@code Codec}
     */
    def dispatch[B](typeFunction: B => T, codec: T => Codec[_ <: B]): Codec[B] =
        this.dispatch("type", "value", typeFunction, codec)

    /**
     * Creates a {@code Codec} for objects that have a type,
     * where objects of different types can have individual {@code Codec}s themselves.
     * The {@code Codec} that this method is called on will be used to encode the type.
     *
     * This is especially useful in combination with a {@link Registry}.
     *
     * The new {@code Codec}'s lifecycle will be {@code Stable}.
     *
     * @param codec Function that gets a {@code Codec} for the value from the type.
     * @tparam B Type for the new {@code Codec}.
     * @return The new {@code Codec}
     */
    def dispatchStable[B](typeFunction: B => T, codec: T => Codec[_ <: B]): Codec[B] =
        this.dispatch("type", "value", typeFunction, codec, Lifecycle.Stable).stable

    /**
     * Creates a new {@code Codec} that is the same as {@code this}, but has a different {@link Lifecycle}.
     *
     * @param l The {@code Lifecycle} used for the new {@code Codec}.
     * @return The new {@code Codec}.
     */
    def withLifecycle(l: Lifecycle): Codec[T] = new Codec[T] {
        def encodeElement(value: T): Result[Element] = self.encodeElement(value).withLifecycle(l)

        def decodeElement(element: Element): Result[T] = self.decodeElement(element).withLifecycle(l)

        val lifecycle: Lifecycle = l
    }

    /**
     * Creates a new {@code Codec} that is the same as {@code this},
     * but has {@code Stable} as its {@link Lifecycle}.
     *
     * @return The new {@code Codec}.
     */
    def stable: Codec[T] = this.withLifecycle(Lifecycle.Stable)

    /**
     * Creates a new {@code Codec} that is the same as {@code this},
     * but has {@code Experimental} as its {@link Lifecycle}.
     *
     * @return The new {@code Codec}.
     */
    def experimental: Codec[T] = this.withLifecycle(Lifecycle.Experimental)

    /**
     * Creates a new {@code Codec} that is the same as {@code this},
     * but has {@code Deprecated} as its {@link Lifecycle}.
     *
     * @param since Since when this has been deprecated.
     * @return The new {@code Codec}.
     */
    def deprecated(since: Int): Codec[T] = this.withLifecycle(Lifecycle.Deprecated(since))
}

object Codec {
    def apply[T](using c: Codec[T]): Codec[T] = c

    given Invariant[Codec] with
        def imap[A, B](codec: Codec[A])(to: A => B)(from: B => A): Codec[B] =
            new Codec[B] {
                def encodeElement(value: B): Result[Element] =
                    codec.encodeElement(from(value))

                override def encodeElementIO[F[_]: Sync](value: B): F[Result[Element]] = for {
                    a <- Sync[F].delay(from(value))
                    encoded <- codec.encodeElementIO(a)
                } yield encoded

                def decodeElement(element: Element): Result[B] =
                    codec.decodeElement(element).map(to)

                override def decodeElementIO[F[_]: Sync](element: Element): F[Result[B]] = for {
                    decoded <- codec.decodeElementIO(element)
                    decodedB <- Sync[F].delay(decoded.map(to))
                } yield decodedB

                override val lifecycle: Lifecycle = codec.lifecycle
            }

    /**
     * Builds a new record codec.
     *
     * Example: {{{
     * case class Test(val a: String, val b: Int)
     *
     * given Codec[Test] = Codec.record {
     *     val a = Codec[String].fieldOf("a").forGetter[Test](_.a)
     *     val b = Codec[Int].fieldOf("b").forGetter[Test](_.b)
     *
     *     Codec.build(Test(a.get, b.get))
     * }
     * }}}
     *
     * @param builder Function where the fields are defined.
     * @tparam T Type of the object.
     * @return The new {@code Codec}
     */
    def record[T](builder: Buffer[FieldCodec[_, T]] ?=> (Buffer[FieldCodec[_, T]], ((FieldCodec[_, T] => _) ?=> T))): Codec[T] = {
        val buildTuple = builder(using ListBuffer[FieldCodec[_, T]]())
        val fields = buildTuple._1.toList
        val creator: (FieldCodec[_, T] => _) ?=> T = buildTuple._2

        new RecordCodec(fields, creator)
    }

    /**
     * Used in {@code Codec.record} to build the instance of the object using the fields.
     *
     * @see {@link record record()}
     * @param builder Function in which the object should be created.
     * @param fields  Context parameter of {@code Codec.record}.
     * @tparam T Type of the object.
     * @return a function that {@code Codec.record} uses to build the object when decoding.
     */
    def build[T](builder: (FieldCodec[_, T] => _) ?=> T)(using fields: Buffer[FieldCodec[_, T]]): (Buffer[FieldCodec[_, T]], (FieldCodec[_, T] => _) ?=> T) =
        (fields, builder)

    /**
     * Creates a unit codec.
     *
     * @param value Value that gets returned when decoding
     * @tparam T Type of the object.
     * @return The unit codec.
     */
    def unit[T](value: T) = new UnitCodec(Left(value))

    /**
     * Creates a unit codec.
     *
     * @param value Value that gets returned when decoding
     * @tparam T Type of the object.
     * @return The unit codec.
     */
    def unit[T](value: () => T) = new UnitCodec(Right(value))

    /**
     * Instance of {@code Codec} for {@code Unit}.
     */
    given Codec[Unit] = Codec.unit(())

    /**
     * Instance of {@code Codec} for {@code Int}.
     */
    given Codec[Int] = new PrimitiveCodec(IntElement(_), _ match {
        case IntElement(value) => Success(value)
        case element => Failure(List(NotAnInt(element, List.empty)))
    })

    /**
     * Instance of {@code Codec} for {@code Long}.
     */
    given Codec[Long] = new PrimitiveCodec(LongElement(_), _ match {
        case IntElement(value) => Success(value.toLong)
        case LongElement(value) => Success(value)
        case element => Failure(List(NotALong(element, List.empty)))
    })

    /**
     * Instance of {@code Codec} for {@code Float}.
     */
    given Codec[Float] = new PrimitiveCodec(FloatElement(_), _ match {
        case FloatElement(value) => Success(value)
        case element => Failure(List(NotAFloat(element, List.empty)))
    })

    /**
     * Instance of {@code Codec} for {@code Double}.
     */
    given Codec[Double] = new PrimitiveCodec(DoubleElement(_), _ match {
        case FloatElement(value) => Success(value.toDouble)
        case DoubleElement(value) => Success(value)
        case element => Failure(List(NotADouble(element, List.empty)))
    })

    /**
     * Instance of {@code Codec} for {@code Boolean}.
     */
    given Codec[Boolean] = new PrimitiveCodec(BooleanElement(_), _ match {
        case BooleanElement(value) => Success(value)
        case element => Failure(List(NotABoolean(element, List.empty)))
    })

    /**
     * Instance of {@code Codec} for {@code String}.
     */
    given Codec[String] = new PrimitiveCodec(StringElement(_), _ match {
        case StringElement(value) => Success(value)
        case element => Failure(List(NotAString(element, List.empty)))
    })

    /**
     * Instance of {@code Codec} for {@code Option[T}}.
     *
     * @tparam T Type of the object in the {@code Option}. Has to have a {@code Codec} as well.
     */
    given[T: Codec]: Codec[Option[T]] = OptionCodec[T]

    /**
     * Creates an instance of {@code Codec} for {@link Either}.
     *
     * @param errorMessage Error message for when none of the two {@code Codec}s match.
     *                     The argument is the path to the erroring element.
     * @tparam L Type of the {@code Left} object.
     * @tparam R Type of the {@code Right} object.
     * @return The {@code Either} codec.
     */
    def either[L: Codec, R: Codec](errorMessage: String => String): Codec[Either[L, R]] = EitherCodec[L, R](errorMessage)

    /**
     * Creates an instance of {@code Codec} for {@link Either}.
     *
     * @param errorMessage Error message for when none of the two {@code Codec}s match.
     * @tparam L Type of the {@code Left} object.
     * @tparam R Type of the {@code Right} object.
     * @return The {@code Either} codec.
     */
    def either[L: Codec, R: Codec](errorMessage: String): Codec[Either[L, R]] = EitherCodec[L, R](
        path => path + ": " + errorMessage)

    /**
     * Creates an instance of {@code Codec} for {@link Either}.
     *
     * @param left  Name for the {@code Left} object. Used in the error message.
     * @param right Name for the {@code Right} object. Used in the error message.
     * @tparam L Type of the {@code Left} object.
     * @tparam R Type of the {@code Right} object.
     * @return The {@code Either} codec.
     */
    def either[L: Codec, R: Codec](left: String, right: String): Codec[Either[L, R]] = EitherCodec[L, R](
        path => s"$path is neither $left nor $right")

    /**
     * Instance of {@code Codec} for {@code Either[L, R]}.
     *
     * @see The {@code Codec.either} methods
     * @param error {@code EitherError} object with the error message.
     * @tparam L Type of the {@code Left} object. Has to have a {@code Codec} as well.
     * @tparam R Type of the {@code Right} object. Has to have a {@code Codec} as well.
     */
    given[L: Codec, R: Codec](using error: EitherError): Codec[Either[L, R]] = EitherCodec[L, R](error.message)

    /**
     * Instance of {@code Codec} for {@code List[T}}.
     *
     * @tparam T Type of the object in the {@code List}. Has to have a {@code Codec} as well.
     */
    given[T: Codec]: Codec[List[T]] = ArrayCodec[T]

    /**
     * Can be used to derive an instance of {@code Codec} for product types, like case classes.
     *
     * @param inst      Context parameter used for derivation
     * @param labelling Context parameter used for derivation
     * @tparam A The type that the instance will be derived for
     * @return The derived {@code Codec}
     */
    inline def derived[A](using inst: K0.ProductGeneric[A], labelling: Labelling[A]): Codec[A] =
        new DerivedCodec
}

trait IncompleteFieldCodec[T](val fieldName: String) extends Codec[T] {
    self =>

    /**
     * Defines how the value of a field is got out of an object.
     *
     * @param getter The function that gets the field.
     * @param fields Context parameter of {@code Codec.record}.
     * @tparam B Type of the object.
     * @return The field codec
     */
    def forGetter[B](getter: B => T)(using fields: Buffer[FieldCodec[_, B]]): FieldCodec[T, B] = {
        val codec = new FieldCodec[T, B](this.fieldName, getter) {
            def encodeElement(value: T): Result[Element] =
                self.encodeElement(value)

            def decodeElement(element: Element): Result[T] =
                self.decodeElement(element)

            override val lifecycle: Lifecycle = self.lifecycle
        }

        fields.addOne(codec)
        codec
    }

    /**
     * Creates a record codec with only one field.
     * The field name is the one used when creating the incomplete field codec.
     *
     * @param to   Function that turns an object of type {@code T} to an object of the new type.
     * @param from Function that turns an object of the new type back to an object of type {@code T}.
     * @tparam B The new type.
     * @return The created {@code Codec}
     */
    override def xmap[B](to: T => B)(from: B => T): Codec[B] =
        new Codec[B] {
            def encodeElement(value: B): Result[Element] =
                self.encodeElement(from(value)).map(result => ObjectElement(Map(self.fieldName -> result)))

            override def encodeElementIO[F[_]: Sync](value: B): F[Result[Element]] = for {
                encoded <- self.encodeElementIO(from(value))
                encodedObject <- Sync[F].delay(encoded.map(result => ObjectElement(Map(self.fieldName -> result))))
            } yield encodedObject

            def decodeElement(element: Element): Result[B] = element match {
                case Element.ObjectElement(map) =>
                    self.decodeElement(map.get(self.fieldName).getOrElse(return Failure(List(MissingKey(element,
                        List(ElementNode.Name(self.fieldName)))), this.lifecycle))).mapBoth(_.map(_.withPrependedPath(self.fieldName)))(to)

                case _ => Failure(List(NotAnObject(element, List.empty)), this.lifecycle)
            }

            override def decodeElementIO[F[_]: Sync](element: Element): F[Result[B]] = element match {
                case ObjectElement(map) => for {
                    mapElement <- Sync[F].delay(map.get(self.fieldName).getOrElse(return Sync[F].pure(
                        Failure[List[ElementError], B](List(MissingKey(element, List(ElementNode.Name(self.fieldName)))),
                            this.lifecycle))))
                    decoded <- self.decodeElementIO(mapElement)
                    decodedWithPath <- Sync[F].delay(decoded.mapBoth(_.map(_.withPrependedPath(self.fieldName)))(to))
                } yield decodedWithPath

                case _ => Sync[F].pure(Failure(List(NotAnObject(element, List.empty)), this.lifecycle))
            }

            override val lifecycle: Lifecycle = self.lifecycle
        }
}

trait FieldCodec[T, B](val fieldName: String, val getter: B => T) extends Codec[T] {
    /**
     * Gets the field value.
     *
     * @param context Context parameter of {@code Codec.build}, used in {@code Codec.record}.
     * @return The field value
     */
    def get(using context: FieldCodec[_, B] => _): T = context(this).asInstanceOf[T]

    def apply(using FieldCodec[_, B] => _): T = get

    def getField(b: B): Result[T] = try {
        getter(b) match {
            case null => Failure(List(NullElementError(List.empty)))
            case value => Success(value)
        }
    } catch {
        case e: Exception => Failure(List(ValidationError(path => s"Exception thrown at $path: $e", List.empty)))
    }
}
