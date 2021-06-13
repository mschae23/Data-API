package de.martenschaefer.data.registry

import de.martenschaefer.data.serialization.Codec
import de.martenschaefer.data.util.Identifier

case class RegistryKey(registry: Identifier, value: Identifier) {
  override def toString: String = s"RegistryKey($registry / $value)"
}

object RegistryKey {
  given [T](using registry: RegistryKey): Codec[RegistryKey] = Codec[Identifier].xmap(RegistryKey(registry, _))(_.value)

  def apply(registry: RegistryKey, value: Identifier): RegistryKey = RegistryKey(registry.value, value)
}
