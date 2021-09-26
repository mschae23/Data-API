package de.martenschaefer.data.test.command

import de.martenschaefer.data.command.Command
import de.martenschaefer.data.command.argument.CommandArgument
import de.martenschaefer.data.command.builder.CommandBuilder.*
import de.martenschaefer.data.test.UnitSpec

class CommandFlagsTest extends UnitSpec {
    val command: Command[String] = Command.build {
        literal("do") {
            withFlag("good") { good =>
                literal("something") {
                    result(if (good) "Did something well" else "Did something")
                }
            }

            literalFlag("nothing") {
                result("Did nothing")
            }
        }

        literal("test") {
            literal("nothing") {
                result("Didn't test anything")
            }

            argumentFlag("test-name", Some('n'), CommandArgument.string("test name")) { testName =>
                result(s"Tested $testName")
            }
        }
    }

    "a command" should "work correctly with correct input (1)" in {
        command.run(List("do", "something")) shouldBe Some("Did something")
    }

    it should "work correctly with correct input (2)" in {
        command.run(List("do", "something", "--good")) shouldBe Some("Did something well")
    }

    it should "work correctly with correct input (3)" in {
        command.run(List("do", "--nothing")) shouldBe Some("Did nothing")
    }

    it should "work correctly with correct input (4)" in {
        command.run(List("test", "nothing")) shouldBe Some("Didn't test anything")
    }

    it should "work correctly with correct input (5)" in {
        command.run(List("test", "--test-name", "commands")) shouldBe Some("Tested commands")
    }

    it should "work correctly with correct input (6)" in {
        command.run(List("test", "--test-name=commands")) shouldBe Some("Tested commands")
    }

    it should "work correctly with correct input (7)" in {
        command.run(List("test", "-n", "commands")) shouldBe Some("Tested commands")
    }

    it should "work correctly with correct input (8)" in {
        command.run(List("test", "-n=commands")) shouldBe Some("Tested commands")
    }

    it should "fail with incorrect input (1)" in {
        command.run(List("do", "whatever")) shouldBe None
    }

    it should "fail with incorrect input (2)" in {
        command.run(List("do", "nothing")) shouldBe None
    }

    it should "fail with incorrect input (3)" in {
        command.run(List("do")) shouldBe None
    }

    it should "fail with incorrect input (4)" in {
        command.run(List("test")) shouldBe None
    }

    it should "fail with incorrect input (5)" in {
        command.run(List("test", "something")) shouldBe None
    }
}
