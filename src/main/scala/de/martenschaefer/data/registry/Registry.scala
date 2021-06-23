package de.martenschaefer.data.registry

import de.martenschaefer.data.serialization.{ Codec, Element, ElementError, ElementNode, RecordParseError, Result, ValidationError }
import de.martenschaefer.data.serialization.codec.RegistryElementCodec
import de.martenschaefer.data.util.DataResult._
import de.martenschaefer.data.util.{ Identifier, Lifecycle }

trait Registry[T](override val lifecycle: Lifecycle) extends Codec[T] {
    /**
     * Registers the given object.
     *
     * @param id The {@link Identifier} of the object.
     * @param t  The object to register.
     * @throws IllegalStateException if an object with that {@code Identifier} was already registered.
     */
    def register(id: Identifier, t: T): Unit

    /**
     * Can be used to set the object at an {@link Identifier} to a different object
     * after it has been registered.
     *
     * @param id The {@link Identifier} of the object.
     * @param t  The object to set.
     * @throws IllegalStateException if an object with that {@code Identifier} was not already registered.
     */
    def set(id: Identifier, t: T): Unit

    /**
     * Removes the object with the given {@link Identifier} from the registry.
     *
     * @param id The {@link Identifier} of the object.
     * @return The object that was removed, or {@code None} if there was no object with that {@code Identifier}.
     */
    def remove(id: Identifier): Option[T]

    /**
     * Gets the object with the given {@link Identifier}.
     *
     * @param id The {@link Identifier} of the object.
     * @return The object, or {@code None} if there was no object with that {@code Identifier}.
     */
    def get(id: Identifier): Option[T]

    /**
     * Gets the {@link Identifier} of a registered object.
     *
     * @param t The object.
     * @return The {@code Identifier} of the object, or {@code None} if that object was not registered.
     */
    def getId(t: T): Option[Identifier]

    /**
     * Gets the {@link RegistryKey} of a registered object.
     *
     * @see {@link getId getId(T)}
     * @param t The object.
     * @return The {@code RegistryKey} of the object, or {@code None} if that object was not registered.
     */
    def getKey(t: T): Option[RegistryKey]

    /**
     * Checks if this registry is empty.
     *
     * @return {@code true} if it is empty, and {@code false} otherwise.
     */
    def isEmpty: Boolean

    /**
     * Checks if this registry contains an object with the given {@link Identifier}.
     *
     * @param id The {@code Identifier} of the object.
     * @return {@code true} if it contains it, and {@code false} otherwise.
     */
    def contains(id: Identifier): Boolean

    /**
     * @return A {@link Map} of this registry's elements.
     */
    def values: Map[Identifier, T]

    /**
     * @return The name of this registry
     */
    def getName: Identifier

    /**
     * Creates a {@link Codec} that can encode and decode values either by registry ID or with the element {@code Codec}.
     * @param elementCodec The {@code Codec} to use when an object is not in the registry.
     * @return The created {@code Codec}.
     */
    def createCodec(elementCodec: Codec[T]): Codec[T] = new RegistryElementCodec[T](this, elementCodec)

    override def encodeElement(value: T): Result[Element] =
        Codec[Identifier].encodeElement(this.getId(value).getOrElse(
            return Failure(Vector(Registry.UnknownRegistryElementError(value)), this.lifecycle)))

    override def decodeElement(element: Element): Result[T] = {
        Codec[Identifier].decodeElement(element) match {
            case Success(id, l) => this.get(id).map(Success(_, this.lifecycle + l)).getOrElse(Failure(Vector(
                Registry.UnknownRegistryIdError(element)), this.lifecycle + l))
            case Failure(errors, l) => Failure(errors, this.lifecycle + l)
        }
    }
}

object Registry {
    def apply[T](using registry: Registry[T]): Registry[T] = registry

    def register[T](id: Identifier, t: T)(using registry: Registry[_ >: T]) =
        registry.register(id, t)

    def set[T](id: Identifier, t: T)(using registry: Registry[_ >: T]) =
        registry.set(id, t)

    extension[T] (t: T)(using registry: Registry[_ >: T])
        def register(id: Identifier) =
            registry.register(id, t)

    case class UnknownRegistryElementError[T](val element: T, override val path: List[ElementNode] = List()) extends ElementError(path) {
        override def getDescription(path: String): String =
            s"$path: Unknown registry element: $element"

        override def withPrependedPath(prependedPath: ElementNode): ElementError =
            UnknownRegistryElementError(this.element, prependedPath :: this.path)
    }

    case class UnknownRegistryIdError(val element: Element, override val path: List[ElementNode] = List()) extends ElementError(path) {
        override def getDescription(path: String): String =
            s"$path: Unknown registry ID: $element"

        override def withPrependedPath(prependedPath: ElementNode): ElementError =
            UnknownRegistryIdError(this.element, prependedPath :: this.path)
    }
}
