package de.martenschaefer.data

import de.martenschaefer.data.registry.{ Registry, RegistryKey }
import de.martenschaefer.data.registry.impl.SimpleRegistry
import de.martenschaefer.data.serialization.{ Codec, PropertiesCodecs }
import de.martenschaefer.data.serialization.Element._
import de.martenschaefer.data.util.{ Either, Identifier, Lifecycle }
import PropertiesCodecs.given

object TestMain {
    case class Test1(val test1: String, test2: Int)

    case class Test2(val testValue: Boolean, val testObject: Test1)

    case class Test3(val testObject2: Test1, val floatingPointNumber: Float)

    case class Test4(val some: Either[Test2, Test3])

    case class Test5(val someNumber: Int, optionalConfig: Option[Test4])

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

    given Codec[Test4] = Codec[Either[Test2, Test3]].fieldOf("some").xmap(Test4(_))(_.some)

    given Codec[Test5] = Codec.record {
        val someNumber = Codec[Int].fieldOf("some_number").forGetter[Test5](_.someNumber)
        val optionalConfig = Codec[Option[Test4]].fieldOf("optional_config").forGetter[Test5](_.optionalConfig)

        Codec.build(Test5(someNumber.get, optionalConfig.get))
    }

    def main(args: Array[String]): Unit = {
        encodingDecodingTest()
        println()
        specialCodecsTest()
        println()
        codecDispatchTest()
    }

    def encodingDecodingTest(): Unit = {
        println("# Encoding & decoding Test")

        println(Codec[Test3].lifecycle)

        val test1 = Test4(Either.Left(Test2(false, Test1("Hello!", 5))))
        val test2 = Test4(Either.Right(Test3(Test1("Hello!", 5), 9.3)))

        println("## Encoding Test")
        println(Codec[Test4].encode(test1))
        println(Codec[Test4].encode(test2))

        val testInput =
            """
              some.test_value = false
              some.test_object.test_1 = Something
              some.test_object.test_2 = 3
              """.stripMargin

        val testInput2 = ObjectElement(Map(
            "some" -> ObjectElement(Map(
                "test_object_2" -> ObjectElement(Map(
                    "test_1" -> StringElement("ABC"),
                    "test_2" -> IntElement(13)
                )),
                "floating_point_number" -> FloatElement(37.2)
            ))
        ))

        val decoded1 = Codec[Test4].decode(testInput)
        val decoded2 = Codec[Test4].decodeElement(testInput2)

        println("## Decoding Test")
        decoded1 match {
            case Either.Right(value) => println(value)
            case Either.Left(errors) => for (error <- errors) println(error)
        }
        decoded2 match {
            case Either.Right(value) => println(value)
            case Either.Left(errors) => for (error <- errors) println(error)
        }
    }

    def specialCodecsTest(): Unit = {
        println("# Special Codecs Test")

        println("## Option")

        val optionTestSome = Test5(11, Some(Test4(Either.Left(Test2(false, Test1("Hello!", 5))))))
        val optionTestNone = Test5(108, scala.None)

        println(Codec[Test5].encode(optionTestSome))
        println(Codec[Test5].encode(optionTestNone))

        val optionTestInput1 = ObjectElement(Map(
            "some_number" -> IntElement(99),
            "optional_config" -> ObjectElement(Map(
                "some" -> ObjectElement(Map(
                    "test_object_2" -> ObjectElement(Map(
                        "test_1" -> StringElement("ABC"),
                        "test_2" -> IntElement(13)
                    )),
                    "floating_point_number" -> FloatElement(37.2)
                ))
            ))
        ))

        val optionTestInput2 = ObjectElement(Map(
            "some_number" -> IntElement(99)
        ))

        println(Codec[Test5].decodeElement(optionTestInput1))
        println(Codec[Test5].decodeElement(optionTestInput2))
    }

    def codecDispatchTest(): Unit = {
        import de.martenschaefer.data.registry.Registry.register

        println("# Codec Dispatch Test")

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

        Codec[Feature1].register(Identifier("test", "feature1"))
        Codec[Feature2].register(Identifier("test", "feature2"))

        println(Codec[Feature].encode(Feature1("Hallo")))

        val featureElement = ObjectElement(Map(
            "value" -> StringElement("Test Test"),
            "type" -> StringElement("test:feature1")
        ))

        val featureElement2 = ObjectElement(Map(
            "value" -> ObjectElement(Map(
                "name" -> StringElement("Hello")
            )),
            "type" -> StringElement("test:feature2")
        ))

        println(Codec[Feature].decodeElement(featureElement))
        println(Codec[Feature].decodeElement(featureElement2))
        println(Codec[Feature].lifecycle)
    }
}
