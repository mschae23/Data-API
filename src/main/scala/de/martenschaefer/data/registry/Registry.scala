package de.martenschaefer.data.registry

import de.martenschaefer.data.serialization.{ Codec, Decoded, Element, ElementError }
import de.martenschaefer.data.util.{ Identifier, Lifecycle }
import de.martenschaefer.data.util.Either._

trait Registry[T](override val lifecycle: Lifecycle) extends Codec[T] {
    def register(id: Identifier, t: T): Unit

    def set(id: Identifier, t: T): Unit

    def remove(id: Identifier): Option[T]

    def get(id: Identifier): Option[T]

    def getId(t: T): Option[Identifier]

    def getKey(t: T): Option[RegistryKey]

    def isEmpty: Boolean

    def contains(id: Identifier): Boolean

    def values: Map[Identifier, T]

    def getName: Identifier

    override def encodeElement(value: T): Element =
        Codec[Identifier].encodeElement(this.getId(value).getOrElse(
            throw new IllegalArgumentException(s"Unknown registry element: $value")))

    override def decodeElement(element: Element): Decoded[T] = {
        Codec[Identifier].decodeElement(element) match {
            case Right(id) => this.get(id).map(Right(_)).getOrElse(Left(Vector(ElementError.ValidationError(path =>
                s"$path: Unknown registry ID", element, List()))))
            case Left(errors) => Left(errors)
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
}
