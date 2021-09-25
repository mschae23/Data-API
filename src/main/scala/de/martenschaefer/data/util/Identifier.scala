package de.martenschaefer.data.util

import de.martenschaefer.data.serialization.Codec

case class Identifier(namespace: String, path: String) {
    override def toString() =
        namespace + Identifier.SEPARATOR + path
}

object Identifier {
    given Codec[Identifier] = Codec[String].xmap(Identifier(_))(_.toString)

    val SEPARATOR = ':'

    def withDefaultNamespace(id: String, defaultNamespace: => String): Identifier = {
        val parts = id.split(SEPARATOR)

        if (parts.length != 2)
            parts(1) = defaultNamespace

        Identifier(parts(0), parts(1))
    }

    def apply(id: String): Identifier = {
        withDefaultNamespace(id, throw new IllegalArgumentException(id))
    }

    def applyOption(id: String): Option[Identifier] = {
        Some(withDefaultNamespace(id, return None))
    }

    def createCodec(defaultNamespace: => String): Codec[Identifier] =
        Codec[String].xmap(Identifier.withDefaultNamespace(_, defaultNamespace))(_.toString)
}
