package de.martenschaefer.data.test.util

import de.martenschaefer.data.util.Lifecycle.*
import de.martenschaefer.data.util.{ Lifecycle, Version }
import org.scalatest.freespec.AnyFreeSpec

class LifecycleTest extends AnyFreeSpec {
    "Stable" - {
        "+ Stable = Stable" in {
            assertResult(Stable)(Stable + Stable)
        }

        "+ Internal = Internal" in {
            assertResult(Internal)(Stable + Internal)
        }

        "+ Experimental = Experimental" in {
            assertResult(Experimental)(Stable + Experimental)
        }

        "+ Deprecated = Deprecated" in {
            assertResult(Deprecated(3))(Stable + Deprecated(3))
        }
    }

    "Internal" - {
        "+ Stable = Internal" in {
            assertResult(Internal)(Internal + Stable)
        }

        "+ Internal = Internal" in {
            assertResult(Internal)(Internal + Internal)
        }

        "+ Experimental = Experimental" in {
            assertResult(Experimental)(Internal + Experimental)
        }

        "+ Deprecated = Deprecated" in {
            assertResult(Deprecated(3))(Internal + Deprecated(3))
        }
    }

    "Experimental" - {
        "+ Stable = Experimental" in {
            assertResult(Experimental)(Experimental + Stable)
        }

        "+ Internal = Experimental" in {
            assertResult(Experimental)(Experimental + Internal)
        }

        "+ Experimental = Experimental" in {
            assertResult(Experimental)(Experimental + Experimental)
        }

        "+ Deprecated = Experimental" in {
            assertResult(Experimental)(Experimental + Deprecated(3))
        }
    }

    "Deprecated" - {
        "+ Stable = Deprecated" in {
            assertResult(Deprecated(Version.Semver(3, 1, 4)))(Deprecated(Version.Semver(3, 1, 4)) + Stable)
        }

        "+ Internal = Deprecated" in {
            assertResult(Deprecated(3))(Deprecated(3) + Internal)
        }

        "+ Experimental = Experimental" in {
            assertResult(Experimental)(Deprecated(3) + Experimental)
        }

        "+ Deprecated = Deprecated" in {
            assertResult(Deprecated(3))(Deprecated(3) + Deprecated(4))

            assertResult(Deprecated(2))(Deprecated(3) + Deprecated(2))

            assertResult(Deprecated(Version.Semver(1, 0, 1)))(
                Deprecated(Version.Semver(1, 0, 1))
                    + Deprecated(Version.Semver(1, 0, 2)))

            assertResult(Deprecated(Version.Semver(1, 0, 1, List("pre", "1"))))(
                Deprecated(Version.Semver(1, 0, 1, List("pre", "1")))
                    + Deprecated(Version.Semver(1, 0, 2)))

            assertResult(Deprecated(Version.Semver(2, 3, 0)))(
                Deprecated(3)
                    + Deprecated(Version.Semver(2, 3, 0)))

            assertResult(Deprecated(Version.Semver(2, 3, 0, List("alpha"))))(
                Deprecated(Version.Semver(2, 3, 0))
                    + Deprecated(Version.Semver(2, 3, 0, List("alpha"))))

            assertResult(Deprecated(2))(
                Deprecated(2) +
                    Deprecated(Version.Semver(2, 3, 0, List("alpha", "2"))))

            assertResult(Deprecated(2))(
                Deprecated(Version.Semver(2, 0, 0)) +
                    Deprecated(Version.Semver(3, 0, 0, List("alpha", "2"))))
        }
    }
}
