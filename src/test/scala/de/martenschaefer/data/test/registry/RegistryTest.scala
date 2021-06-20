package de.martenschaefer.data.test.registry

import de.martenschaefer.data.registry.Registry
import de.martenschaefer.data.registry.impl.SimpleRegistry
import de.martenschaefer.data.serialization.{ Codec, Element, ElementError, RecordParseError, ElementNode }
import de.martenschaefer.data.test.UnitSpec
import de.martenschaefer.data.util._
import de.martenschaefer.data.util.Either._
import Registry.register

class RegistryTest extends UnitSpec {
    given Registry[Int] = new SimpleRegistry(Identifier("test", "int"))

    "A registry" should "contain an element after registering it" in {
        val id = Identifier("test", "one")

        1.register(id)

        assert(Registry[Int].contains(id))
        assertResult(Some(1))(Registry[Int].get(id))
    }

    it should "not contain an element after removing it" in {
        val id = Identifier("test", "two")

        2.register(id)
        Registry[Int].remove(id)

        assert(!Registry[Int].contains(id), "Registry contained removed element")
    }

    it should "not allow an element to be registered twice" in {
        assertThrows[IllegalStateException] {
            val id = Identifier("test", "three")

            3.register(id)
            3.register(id)
        }
    }

    it should "not allow an unregistered element to be set" in {
        assertThrows[IllegalStateException] {
            val id = Identifier("test", "four")

            Registry[Int].set(id, 4)
        }
    }

    it should "have a working getId method" in {
        val id = Identifier("test", "five")

        5.register(id)

        assertResult(Some(id))(Registry[Int].getId(5))
    }

    val intCodec = Registry[Int].createCodec(Codec.given_Codec_Int) // Codec[Int] refers to the registry

    "A RegistryElementCodec" should "encode objects in the registry correctly" in {
        val id = Identifier("test", "six")

        6.register(id)

        intCodec.encodeElement(6) shouldBe Right(Element.StringElement("test:six"))
    }

    it should "encode objects not in the registry correctly" in {
        intCodec.encodeElement(7) shouldBe Right(Element.IntElement(7))
    }

    it should "decode objects in the registry correctly" in {
        val id = Identifier("test", "eight")

        8.register(id)

        intCodec.decodeElement(Element.StringElement("test:eight")) shouldBe Right(8)

        intCodec.decodeElement(Element.IntElement(8)) shouldBe Right(8)
    }

    it should "decode objects not in the registry correctly" in {
        intCodec.decodeElement(Element.StringElement("test:nine")) shouldBe
            Left(Vector(RecordParseError.NotAnInt(Element.StringElement("test:nine"), List())))

        intCodec.decodeElement(Element.IntElement(9)) shouldBe Right(9)
    }
}
