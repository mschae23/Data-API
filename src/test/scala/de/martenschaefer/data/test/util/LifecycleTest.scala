package de.martenschaefer.data.test.util

import org.scalatest.freespec.AnyFreeSpec
import de.martenschaefer.data.util.Lifecycle
import de.martenschaefer.data.util.Lifecycle._

class LifecycleTest extends AnyFreeSpec {
    "Stable" - {
        "+ Stable = Stable" in {
            assertResult(Stable)(Stable + Stable)
        }

        "+ Experimental = Experimental" in {
            assertResult(Experimental)(Stable + Experimental)
        }

        "+ Deprecated = Deprecated" in {
            assertResult(Deprecated(3))(Stable + Deprecated(3))
        }
    }

    "Experimental" - {
        "+ Stable = Experimental" in {
            assertResult(Experimental)(Experimental + Stable)
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
            assertResult(Deprecated(3))(Deprecated(3) + Stable)
        }

        "+ Experimental = Experimental" in {
            assertResult(Experimental)(Deprecated(3) + Experimental)
        }

        "+ Deprecated = Deprecated" in {
            assertResult(Deprecated(3))(Deprecated(3) + Deprecated(4))

            assertResult(Deprecated(2))(Deprecated(3) + Deprecated(2))
        }
    }
}
