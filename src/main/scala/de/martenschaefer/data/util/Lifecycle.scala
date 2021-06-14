package de.martenschaefer.data.util

import de.martenschaefer.data.serialization.{ Codec, ElementError }
import de.martenschaefer.data.util.Either._

enum Lifecycle {
    case Stable
    case Experimental
    case Deprecated(since: Int)

    def +(other: Lifecycle): Lifecycle =
        this match {
            case Lifecycle.Experimental => Lifecycle.Experimental
            case _ if other == Lifecycle.Experimental => Lifecycle.Experimental
            case Lifecycle.Deprecated(since) if other.isInstanceOf[Lifecycle.Deprecated]
              && other.asInstanceOf[Lifecycle.Deprecated].since < since => other
            case Lifecycle.Deprecated(_) => this
            case _ if other.isInstanceOf[Lifecycle.Deprecated] => other
            case _ => Lifecycle.Stable
        }
}

object Lifecycle {
    private val errorMsg = (path: String) =>
        s"$path should be \"stable\", \"experimental\" or an object with a \"deprecated_since\" field"

    given Codec[Lifecycle.Deprecated] = Codec[Int].fieldOf("deprecated_since").xmap[Lifecycle.Deprecated](Lifecycle.Deprecated(_))(_.since)

    given Codec[Lifecycle] = Codec.either[String, Lifecycle.Deprecated](errorMsg).flatXmap((either, element) => either match {
        case Left(name) => name match {
            case "stable" => Right(Lifecycle.Stable)
            case "experimental" => Right(Lifecycle.Experimental)
            case _ => Left(Vector(ElementError.ValidationError(errorMsg, element, List())))
        }
        case Right(deprecated) => Right(deprecated)
    })(_ match {
        case Lifecycle.Stable => Left("stable")
        case Lifecycle.Experimental => Left("experimental")
        case Lifecycle.Deprecated(since) => Right(Lifecycle.Deprecated(since))
    })
}
