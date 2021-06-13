package de.martenschaefer.data

import de.martenschaefer.data.registry.{ Registry, RegistryKey }
import de.martenschaefer.data.registry.impl.SimpleRegistry
import de.martenschaefer.data.serialization.{ Codec, PropertiesCodecs }
import de.martenschaefer.data.serialization.Element._
import de.martenschaefer.data.util.{ Either, Identifier, Lifecycle }
import PropertiesCodecs.given

object TestMain {
    def main(args: Array[String]): Unit = {
        case class Test(val test1: String, test2: Int)

        val testCodec = Codec.record[Test] {
            val test1 = Codec[String].fieldOf("test1").forGetter[Test](_.test1)
            val test2 = Codec[Int].fieldOf("test2").forGetter[Test](_.test2)

            Codec.build(Test(test1.get, test2.get))
        }

        case class Test2(val testValue: Boolean, val testObject: Test)

        val test2Codec = Codec.record[Test2] {
            val testValue = Codec[Boolean].fieldOf("testValue").forGetter[Test2](_.testValue)
            val testObject = testCodec.fieldOf("testObject").forGetter[Test2](_.testObject)

            Codec.build(Test2(testValue.get, testObject.get))
        }

        case class Test3(val some: Test2)

        val test3Codec = test2Codec.fieldOf("some").xmap(Test3(_))(_.some)
        println(test3Codec.lifecycle)

        val test3 = Test3(Test2(false, Test("Hello!", 5)))

        println(test3Codec.encode(test3))

        val testInput =
            """
              some.testValue = false
              some.testObject.test1 = Something
              some.testObject.test2 = 3
              """.stripMargin

        val decoded = test3Codec.decode(testInput)

        decoded match {
            case Either.Right(value) => println(value)
            case Either.Left(errors) => for (error <- errors) println(error)
        }

        println()
        codecDispatchTest()
    }

    def codecDispatchTest(): Unit = {
        import de.martenschaefer.data.registry.Registry.register

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
            given Codec[Feature1] = Codec[String].fieldOf("name").xmap(Feature1(_))(_.name)
        }

        case class Feature2(val something: String) extends Feature {
            override def getCodec: Codec[Feature2] = Codec[Feature2]

            override def getString: String = s"Something: $something"
        }

        object Feature2 {
            given Codec[Feature2] = Codec[String].fieldOf("something").xmap(Feature2(_))(_.something)
        }

        object Feature {
            given Codec[Feature] = Registry[Codec[_ <: Feature]].dispatch(_.getCodec, c => c)
        }

        Codec[Feature1].register(Identifier("test", "feature1"))
        Codec[Feature2].register(Identifier("test", "feature2"))

        println(Codec[Feature].encode(Feature1("Hallo")))

        val featureElement = ObjectElement(Map(
            "name" -> StringElement("Test Test"),
            "type" -> StringElement("test:feature1")
        ))

        println()
        println(Codec[Feature].decodeElement(featureElement))
    }
}
