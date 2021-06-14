package de.martenschaefer.data.test.serialization

import org.scalatest.BeforeAndAfterAll
import de.martenschaefer.data.registry.Registry
import de.martenschaefer.data.registry.impl.SimpleRegistry
import de.martenschaefer.data.serialization.{ Codec, ElementNode }
import de.martenschaefer.data.serialization.Element._
import de.martenschaefer.data.serialization.ElementError._
import de.martenschaefer.data.test.UnitSpec
import de.martenschaefer.data.util.{ Either, Identifier, Lifecycle }
import de.martenschaefer.data.util.Either._

class CodecDispatchTest extends UnitSpec, BeforeAndAfterAll {
    trait Feature {
        def getCodec: Codec[_ <: Feature]

        def getString: String
    }

    given Registry[Codec[_ <: Feature]] = new SimpleRegistry(Identifier("test", "feature"), Lifecycle.Experimental)

    case class Feature1(val name: String) extends Feature {
        override def getCodec: Codec[Feature1] = Codec[Feature1]

        override def getString: String = this.name
    }

    object Feature1 {
        given Codec[Feature1] = Codec[String].xmap(Feature1(_))(_.name)
    }

    case class Feature2(val name: String) extends Feature {
        override def getCodec: Codec[Feature2] = Codec[Feature2]

        override def getString: String = s"Name: $name"
    }

    object Feature2 {
        given Codec[Feature2] = Codec[String].fieldOf("name").xmap(Feature2(_))(_.name)
    }

    object Feature {
        given Codec[Feature] = Registry[Codec[_ <: Feature]].dispatch(_.getCodec, c => c)
    }

    override def beforeAll(): Unit = {
        import Registry.register

        Codec[Feature1].register(Identifier("test", "feature1"))
        Codec[Feature2].register(Identifier("test", "feature2"))
    }

    "Dispatched Codecs" should "correctly encode objects (1)" in {
        assertResult(ObjectElement(Map(
            "value" -> StringElement("Hello!"),
            "type" -> StringElement("test:feature1")
        ))) {
            Codec[Feature].encodeElement(Feature1("Hello!"))
        }
    }

    they should "correctly encode objects (2)" in {
        assertResult(ObjectElement(Map(
            "name" -> StringElement("Feature Name"),
            "type" -> StringElement("test:feature2")
        ))) {
            Codec[Feature].encodeElement(Feature2("Feature Name"))
        }
    }

    they should "correctly decode elements (1)" in {
        val featureElement = ObjectElement(Map(
            "value" -> StringElement("Test Test"),
            "type" -> StringElement("test:feature1")
        ))

        assertResult(Right(Feature1("Test Test"))) {
            Codec[Feature].decodeElement(featureElement)
        }
    }

    they should "correctly decode elements (2)" in {
        val featureElement = ObjectElement(Map(
            "value" -> ObjectElement(Map(
                "name" -> StringElement("Feature 2")
            )),
            "type" -> StringElement("test:feature2")
        ))

        assertResult(Right(Feature2("Feature 2"))) {
            Codec[Feature].decodeElement(featureElement)
        }
    }

    they should "correctly decode elements (3)" in {
        val featureElement = ObjectElement(Map(
            "name" -> StringElement("Something"),
            "type" -> StringElement("test:feature2")
        ))

        assertResult(Right(Feature2("Something"))) {
            Codec[Feature].decodeElement(featureElement)
        }
    }

    they should "inherit their lifecycle from the original codec by default" in {
        assertResult(Lifecycle.Experimental)(Codec[Feature].lifecycle)
    }
}
