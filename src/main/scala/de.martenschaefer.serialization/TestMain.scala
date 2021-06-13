package de.martenschaefer.serialization

import de.martenschaefer.serialization.util.Either

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

        val test3 = Test3(Test2(false, Test("Hello!", 5)))

        import PropertiesCodecs.given

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
    }
}
