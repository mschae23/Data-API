package de.martenschaefer.data.test.serialization

import de.martenschaefer.data.serialization.{ Codec, ElementNode }
import de.martenschaefer.data.serialization.Element._
import de.martenschaefer.data.serialization.ElementError._
import de.martenschaefer.data.test.UnitSpec
import de.martenschaefer.data.util.{ Either, Lifecycle }
import de.martenschaefer.data.util.Either._
import de.martenschaefer.data.test.UnitSpec

class DerivedCodecTest extends UnitSpec {
    case class Test1(val test1: String, test2: Int) derives Codec

    case class Test2(val testValue: Boolean, val testObject: Test1) derives Codec

    case class Test3(val testObject2: Test1, val floatingPointNumber: Float) derives Codec

    case class Test4(val some: Either[Test2, Test3]) derives Codec

    case class Test5(val lifecycle: Lifecycle, optionalConfig: Option[Test4]) derives Codec

    case class Test6(val number: Long, val list: List[Test1]) derives Codec

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

    val test6Element = ObjectElement(Map(
        "number" -> LongElement(4L),
        "list" -> ArrayElement(List(
            ObjectElement(Map(
                "test_1" -> StringElement("a"),
                "test_2" -> IntElement(5)
            )),
            ObjectElement(Map(
                "test_1" -> StringElement("b"),
                "test_2" -> IntElement(99)
            ))
        ))
    ))

    val test6Object = Test6(4L, List(Test1("a", 5), Test1("b", 99)))

    "A derived Codec" should "encode an object to an Element" in {
        assertResult(Right(test5Element)) {
            Codec[Test5].encodeElement(test5Object)
        }
    }

    it should "encode an object to an Element (3)" in {
        assertResult(Right(test6Element)) {
            Codec[Test6].encodeElement(test6Object)
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

    it should "decode an Element to an object (4)" in {
        assertResult(Right(test6Object)) {
            Codec[Test6].decodeElement(test6Element)
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

    it should "inherit its lifecycle from its fields" in {
        assertResult(Lifecycle.Deprecated(3)) {
            Codec.record {
                val test1 = Codec[String].fieldOf("test_1").forGetter[Test1](_.test1)
                val test2 = Codec[Int].deprecated(3).fieldOf("test_2").forGetter[Test1](_.test2)

                Codec.build(Test1(test1.get, test2.get))
            }.lifecycle
        }
    }
}
