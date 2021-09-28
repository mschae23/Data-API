package de.martenschaefer.data.util

import de.martenschaefer.data.serialization.{ Codec, EitherError, RecordParseError }
import de.martenschaefer.data.util.DataResult._

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
    private given EitherError =
        EitherError(path => s"$path should be \"stable\", \"experimental\" or an object with a \"deprecated_since\" field")

    given Codec[Lifecycle.Deprecated] = Codec[Int].fieldOf("deprecated_since").xmap[Lifecycle.Deprecated](Lifecycle.Deprecated(_))(_.since)

    given Codec[Lifecycle] = Codec[Either[String, Lifecycle.Deprecated]].flatXmap((either, element) => either match {
        case Left(name) => name match {
            case "stable" => Success(Lifecycle.Stable)
            case "experimental" => Success(Lifecycle.Experimental)
            case _ => Failure(List(RecordParseError.ValidationParseError(EitherError.message, element, List.empty)))
        }
        case Right(deprecated) => Success(deprecated)
    })(_ match {
        case Lifecycle.Stable => Success(Left("stable"))
        case Lifecycle.Experimental => Success(Left("experimental"))
        case Lifecycle.Deprecated(since) => Success(Right(Lifecycle.Deprecated(since)))
    })
}
