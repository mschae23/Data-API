package de.martenschaefer.data.util

import de.martenschaefer.data.serialization.{ Codec, EitherError, ValidationError }
import de.martenschaefer.data.util.DataResult.*

enum Lifecycle {
    case Stable
    case Internal
    case Experimental
    case Deprecated(since: Version)

    def +(other: Lifecycle): Lifecycle =
        this match {
            case Lifecycle.Experimental => Lifecycle.Experimental
            case _ if other == Lifecycle.Experimental => Lifecycle.Experimental
            case Lifecycle.Deprecated(since) if other.isInstanceOf[Lifecycle.Deprecated]
                && other.asInstanceOf[Lifecycle.Deprecated].since < since => other
            case Lifecycle.Deprecated(_) => this
            case _ if other.isInstanceOf[Lifecycle.Deprecated] => other
            case Lifecycle.Internal => this
            case _ if other == Lifecycle.Internal => other
            case _ => Lifecycle.Stable
        }
}

object Lifecycle {
    object Deprecated {
        def apply(since: Version): Deprecated = new Deprecated(since)

        def apply(since: Int): Deprecated = Deprecated(Version.Simple(since))
    }

    given Codec[Lifecycle.Deprecated] = Codec[Version].fieldOf("deprecated_since").xmap[Lifecycle.Deprecated](Lifecycle.Deprecated(_))(_.since)

    private val deprecatedCodec: Codec[Lifecycle] = Codec[Lifecycle.Deprecated].flatXmap(Success(_)) {
        case deprecated: Lifecycle.Deprecated => Success(deprecated)
        case _ => Failure(List(ValidationError(path => s"$path: Lifecycle is not deprecated", List.empty)))
    }

    private val stableOrExperimentalCodec: Codec[Lifecycle] = Codec[String].flatXmap {
        case "stable" => Success(Lifecycle.Stable)
        case "experimental" => Success(Lifecycle.Experimental)
        case name => Failure(List(ValidationError(path => s"$path: \"$name\" is neither \"stable\" nor \" experimental\"", List.empty)))
    } {
        case Lifecycle.Stable => Success("stable")
        case Lifecycle.Experimental => Success("experimental")
        case _ => Failure(List(ValidationError(path => s"$path: Lifecycle is not stable or experimental", List.empty)))
    }

    given Codec[Lifecycle] = Codec.alternatives(List(stableOrExperimentalCodec, deprecatedCodec))
}
