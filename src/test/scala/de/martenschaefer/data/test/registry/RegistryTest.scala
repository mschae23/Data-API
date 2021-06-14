package de.martenschaefer.data.test.registry

import de.martenschaefer.data.registry.Registry
import de.martenschaefer.data.registry.impl.SimpleRegistry
import de.martenschaefer.data.test.UnitSpec
import de.martenschaefer.data.util.Identifier
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
}
