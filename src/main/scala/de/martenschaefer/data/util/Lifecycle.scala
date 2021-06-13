package de.martenschaefer.data.util

enum Lifecycle {
    case Stable
    case Experimental
    case Deprecated(since: Int)

    def +(other: Lifecycle): Lifecycle =
        this match {
            case Lifecycle.Experimental => Lifecycle.Experimental
            case _ if other.eq(Lifecycle.Experimental) => Lifecycle.Experimental
            case Lifecycle.Deprecated(since) if other.isInstanceOf[Lifecycle.Deprecated]
              && other.asInstanceOf[Lifecycle.Deprecated].since < since => other
            case Lifecycle.Deprecated(_) => this
            case _ if other.isInstanceOf[Lifecycle.Deprecated] => other
            case _ => Lifecycle.Stable
        }
}
