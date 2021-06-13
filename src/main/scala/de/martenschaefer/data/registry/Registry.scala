package de.martenschaefer.data.registry

import de.martenschaefer.data.util.Identifier

trait Registry[T] {
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
}

object Registry {

    def register[T](id: Identifier, t: T)(using registry: Registry[_ >: T]) =
        registry.register(id, t)

    def set[T](id: Identifier, t: T)(using registry: Registry[_ >: T]) =
        registry.set(id, t)

    extension[T] (t: T)(using registry: Registry[_ >: T])
        def register(id: Identifier) =
            registry.register(id, t)
}
