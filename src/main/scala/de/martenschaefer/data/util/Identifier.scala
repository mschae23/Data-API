package de.martenschaefer.data.util

import de.martenschaefer.data.serialization.Codec

case class Identifier(namespace: String, path: String) {
  override def toString() =
    namespace + Identifier.SEPARATOR + path
}

object Identifier {
  given Codec[Identifier] = Codec[String].xmap(Identifier(_))(_.toString)

  val SEPARATOR = ':'
  
  def apply(string: String): Identifier = {
    val parts = string.split(SEPARATOR)

    if (parts.length != 2) throw new IllegalArgumentException(string)
    
    Identifier(parts(0), parts(1))
  }
}
