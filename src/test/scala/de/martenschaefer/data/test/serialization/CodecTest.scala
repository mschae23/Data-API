package de.martenschaefer.data.test.serialization

import de.martenschaefer.data.serialization.{ Codec, ElementNode }
import de.martenschaefer.data.serialization.Element._
import de.martenschaefer.data.serialization.ElementError._
import de.martenschaefer.data.test.UnitSpec
import de.martenschaefer.data.util.{ Either, Lifecycle }
import de.martenschaefer.data.util.Either._

class CodecTest extends UnitSpec {
    case class Test1(val test1: String, test2: Int)

    case class Test2(val testValue: Boolean, val testObject: Test1)

    case class Test3(val testObject2: Test1, val floatingPointNumber: Float)

    case class Test4(val some: Either[Test2, Test3])

    case class Test5(val lifecycle: Lifecycle, optionalConfig: Option[Test4])

    given Codec[Test1] = Codec.record {
        val test1 = Codec[String].fieldOf("test_1").forGetter[Test1](_.test1)
        val test2 = Codec[Int].fieldOf("test_2").forGetter[Test1](_.test2)

        Codec.build(Test1(test1.get, test2.get))
    }

    given Codec[Test2] = Codec.record {
        val testValue = Codec[Boolean].fieldOf("test_value").forGetter[Test2](_.testValue)
        val testObject = Codec[Test1].fieldOf("test_object").forGetter[Test2](_.testObject)

        Codec.build(Test2(testValue.get, testObject.get))
    }

    given Codec[Test3] = Codec.record {
        val testObject2 = Codec[Test1].fieldOf("test_object_2").forGetter[Test3](_.testObject2)
        val floatingPointNumber = Codec[Float].fieldOf("floating_point_number").forGetter[Test3](_.floatingPointNumber)

        Codec.build(Test3(testObject2.get, floatingPointNumber.get))
    }

    given Codec[Test4] = Codec.either[Test2, Test3]("Test2", "Test3")
        .fieldOf("some").xmap(Test4(_))(_.some)

    given Codec[Test5] = Codec.record {
        val lifecycle = Codec[Lifecycle].fieldOf("lifecycle").forGetter[Test5](_.lifecycle)
        val optionalConfig = Codec[Option[Test4]].fieldOf("optional_config").forGetter[Test5](_.optionalConfig)

        Codec.build(Test5(lifecycle.get, optionalConfig.get))
    }

    val test5Object = Test5(Lifecycle.Stable, Some(Test4(Right(Test3(Test1("Hello!", 8), 3.1f)))))

    val test5Object2 = Test5(Lifecycle.Stable, Some(Test4(Left(Test2(true, Test1("Hello!", 8))))))

    val test5ObjectNone = Test5(Lifecycle.Stable, scala.None)

    val test5Element = ObjectElement(Map(
        "lifecycle" -> StringElement("stable"),
        "optional_config" -> ObjectElement(Map(
            "some" -> ObjectElement(Map(
                "floating_point_number" -> FloatElement(3.1),
                "test_object_2" -> ObjectElement(Map(
                    "test_1" -> StringElement("Hello!"),
                    "test_2" -> IntElement(8)
                ))
            )
            ))
        )))

    val test5Element2 = ObjectElement(Map(
        "lifecycle" -> StringElement("stable"),
        "optional_config" -> ObjectElement(Map(
            "some" -> ObjectElement(Map(
                "test_value" -> BooleanElement(true),
                "test_object" -> ObjectElement(Map(
                    "test_1" -> StringElement("Hello!"),
                    "test_2" -> IntElement(8)
                ))
            ))
        ))
    ))

    "A Codec" should "encode an object to an Element" in {
        assertResult(Right(test5Element)) {
            Codec[Test5].encodeElement(test5Object)
        }
    }

    it should "encode an object to an Element (2)" in {
        assertResult(Right(ObjectElement(Map(
            "test" -> DoubleElement(17.3)
        )))) {
            case class Test(val test: Double)

            Codec[Double].fieldOf("test").xmap(Test(_))(_.test).encodeElement(Test(17.3))
        }
    }

    it should "decode an Element to an object (Option.Some, Right)" in {
        assertResult(Right(test5Object)) {
            Codec[Test5].decodeElement(test5Element)
        }
    }

    it should "decode an Element to an object (Option.Some, Left)" in {
        assertResult(Right(test5Object2)) {
            Codec[Test5].decodeElement(test5Element2)
        }
    }

    it should "decode an Element to an object (Option.None)" in {
        assertResult(Right(test5ObjectNone)) {
            Codec[Test5].decodeElement(ObjectElement(Map(
                "lifecycle" -> StringElement("stable")
            )))
        }
    }

    it should "return the correct \"not a ...\" errors when decoding" in {
        val testElement = ObjectElement(Map(
            "test_value" -> FloatElement(5.5f),
            "test_object" -> ObjectElement(Map(
                "test_1" -> BooleanElement(false),
                "test_2" -> IntElement(8)
            ))
        ))

        assertResult(Left(Vector(
            NotABoolean(FloatElement(5.5f), List(ElementNode.Name("test_value"))),
            NotAString(BooleanElement(false), List(ElementNode.Name("test_object"), ElementNode.Name("test_1")))
        ))) {
            Codec[Test2].decodeElement(testElement)
        }
    }

    it should "return missing key errors when decoding elements with missing keys" in {
        val testElement = ObjectElement(Map(
            "test_object" -> ObjectElement(Map(
                "test_2" -> IntElement(8)
            ))
        ))

        assertResult(Left(Vector(
            MissingKey(testElement, List(ElementNode.Name("test_value"))),
            MissingKey(ObjectElement(Map("test_2" -> IntElement(8))), List(ElementNode.Name("test_object"),
                ElementNode.Name("test_1")))
        ))) {
            Codec[Test2].decodeElement(testElement)
        }
    }

    it should "have Lifecycle.Stable by default" in {
        assertResult(Lifecycle.Stable)(Codec[Test5].lifecycle)
    }

    "A record codec" should "inherit its lifecycle from its fields" in {
        assertResult(Lifecycle.Experimental) {
            Codec.record {
                val test1 = Codec[String].fieldOf("test_1").forGetter[Test1](_.test1)
                val test2 = Codec[Int].experimental.fieldOf("test_2").forGetter[Test1](_.test2)

                Codec.build(Test1(test1.get, test2.get))
            }.lifecycle
        }
    }
}
