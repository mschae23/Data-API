package de.martenschaefer.data.registry

import de.martenschaefer.data.util.Identifier

case class RegistryKey(registry: Identifier, value: Identifier ) {
  override def toString: String = s"RegistryKey($registry / $value)"
}

object RegistryKey {
  def apply(registry: RegistryKey, value: Identifier): Unit = RegistryKey(registry.value, value)
}
