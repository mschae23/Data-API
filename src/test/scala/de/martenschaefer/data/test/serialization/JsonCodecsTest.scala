package de.martenschaefer.data.test.serialization

import de.martenschaefer.data.serialization.{ Codec, JsonCodecs }
import de.martenschaefer.data.test.UnitSpec
import de.martenschaefer.data.util.DataResult._
import JsonCodecs.given

class JsonCodecsTest extends UnitSpec {
    case class Test1(val name: String, val something: Int)
    case class Test2(val a: Boolean, testObject: Test1)

    given Codec[Test1] = Codec.record {
        val name = Codec[String].fieldOf("name").forGetter[Test1](_.name)
        val something = Codec[Int].fieldOf("something").forGetter[Test1](_.something)

        Codec.build(Test1(name.get, something.get))
    }

    given Codec[Test2] = Codec.record {
        val a = Codec[Boolean].fieldOf("a").forGetter[Test2](_.a)
        val testObject = Codec[Test1].fieldOf("test_object").forGetter[Test2](_.testObject)

        Codec.build(Test2(a.get, testObject.get))
    }

    val testValue = Test2(true, Test1("Test Name", 123))
    val testJson = "{\"a\":true,\"test_object\":{\"name\":\"Test Name\",\"something\":123}}"
    val prettyTestJson = "{\n  \"a\": true,\n  \"test_object\": {\n    \"name\": \"Test Name\",\n    \"something\": 123\n  }\n}"

    "The JSON codec" should "encode objects correctly" in {
        assertResult(Success(testJson))(Codec[Test2].encode(testValue))
    }

    it should "encode objects correctly (pretty)" in {
        assertResult(Success(prettyTestJson)) {
            Codec[Test2].encode(testValue)(using JsonCodecs.prettyJsonEncoder)
        }
    }

    it should "decode an object correctly" in {
        assertResult(Success(testValue))(Codec[Test2].decode(testJson))
    }

    it should "decode a pretty object correctly" in {
        assertResult(Success(testValue))(Codec[Test2].decode(prettyTestJson))
    }
}
