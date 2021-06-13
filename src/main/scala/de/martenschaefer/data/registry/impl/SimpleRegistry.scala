package de.martenschaefer.data.registry.impl

import scala.collection.mutable.HashMap
import de.martenschaefer.data.registry.{ Registry, RegistryKey }
import de.martenschaefer.data.util.{ Identifier, Lifecycle }

class SimpleRegistry[T](val name: Identifier, override val lifecycle: Lifecycle) extends Registry[T](lifecycle) {
    def this(name: Identifier) = this(name, Lifecycle.Stable)

    private val elements = HashMap.empty[Identifier, T]

    override def register(id: Identifier, t: T) = {
        if (contains(id)) throw new IllegalStateException(s"Attempted to register element ($id / $name) twice.")
        else elements.put(id, t)
    }

    override def set(id: Identifier, t: T) = {
        if (!contains(id)) throw new IllegalStateException(s"Attempted to set value for unregistered element ($id / $name).")
        else elements.put(id, t)
    }

    override def remove(id: Identifier) =
        elements.remove(id)

    override def get(id: Identifier) =
        elements.get(id)

    override def getId(t: T): Option[Identifier] =
        elements.find((id, element) => element == t)
          .map(entry => entry._1)

    override def getKey(t: T): Option[RegistryKey] = getId(t).map(id => RegistryKey(this.name, id))

    override def isEmpty = elements.isEmpty

    override def contains(id: Identifier) = elements.contains(id)

    override def values: Predef.Map[Identifier, T] = elements.toMap

    override def getName: Identifier = this.name
}
