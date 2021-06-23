package de.martenschaefer.data.test.serialization

import org.scalatest.BeforeAndAfterAll
import de.martenschaefer.data.registry.Registry
import de.martenschaefer.data.registry.impl.SimpleRegistry
import de.martenschaefer.data.serialization.{ Codec, ElementNode }
import de.martenschaefer.data.serialization.Element._
import de.martenschaefer.data.serialization.ElementError._
import de.martenschaefer.data.test.UnitSpec
import de.martenschaefer.data.util.{ DataResult, Identifier, Lifecycle }
import de.martenschaefer.data.util.DataResult._

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

    case class Feature3() extends Feature { // case object doesn't work for some reason
        override def getCodec: Codec[Feature3] = Codec[Feature3]

        override def getString: String = "Some string"
    }

    object Feature3 {
        given Codec[Feature3] = Codec.unit(Feature3())
    }

    object Feature {
        given Codec[Feature] = Registry[Codec[_ <: Feature]].dispatch(_.getCodec, c => c)
    }

    override def beforeAll(): Unit = {
        import Registry.register

        Codec[Feature1].register(Identifier("test", "feature1"))
        Codec[Feature2].register(Identifier("test", "feature2"))
        Codec[Feature3].register(Identifier("test", "feature3"))
    }

    "Dispatched Codecs" should "correctly encode objects (1)" in {
        assertResult(Success(ObjectElement(Map(
            "value" -> StringElement("Hello!"),
            "type" -> StringElement("test:feature1")
        )), Lifecycle.Experimental)) {
            Codec[Feature].encodeElement(Feature1("Hello!"))
        }
    }

    they should "correctly encode objects (2)" in {
        assertResult(Success(ObjectElement(Map(
            "name" -> StringElement("Feature Name"),
            "type" -> StringElement("test:feature2")
        )), Lifecycle.Experimental)) {
            Codec[Feature].encodeElement(Feature2("Feature Name"))
        }
    }

    they should "correctly encode objects (3, unit codec)" in {
        assertResult(Success(ObjectElement(Map(
            "type" -> StringElement("test:feature3")
        )), Lifecycle.Experimental)) {
            Codec[Feature].encodeElement(Feature3())
        }
    }

    they should "correctly decode elements (1)" in {
        val featureElement = ObjectElement(Map(
            "value" -> StringElement("Test Test"),
            "type" -> StringElement("test:feature1")
        ))

        assertResult(Success(Feature1("Test Test"), Lifecycle.Experimental)) {
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

        assertResult(Success(Feature2("Feature 2"), Lifecycle.Experimental)) {
            Codec[Feature].decodeElement(featureElement)
        }
    }

    they should "correctly decode elements (3)" in {
        val featureElement = ObjectElement(Map(
            "name" -> StringElement("Something"),
            "type" -> StringElement("test:feature2")
        ))

        assertResult(Success(Feature2("Something"), Lifecycle.Experimental)) {
            Codec[Feature].decodeElement(featureElement)
        }
    }

    they should "correctly decode elements (4, unit codec)" in {
        val featureElement = ObjectElement(Map(
            "type" -> StringElement("test:feature3")
        ))

        assertResult(Success(Feature3(), Lifecycle.Experimental)) {
            Codec[Feature].decodeElement(featureElement)
        }
    }

    they should "inherit their lifecycle from the original codec by default" in {
        assertResult(Lifecycle.Experimental)(Codec[Feature].lifecycle)
    }
}
