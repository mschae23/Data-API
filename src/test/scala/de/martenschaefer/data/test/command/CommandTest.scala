package de.martenschaefer.data.test.command

import de.martenschaefer.data.command.Command
import de.martenschaefer.data.command.argument.CommandArgument as Argument
import de.martenschaefer.data.command.argument.CommandArgument.{ literal as literalArg, * }
import de.martenschaefer.data.command.builder.CommandBuilder.*
import de.martenschaefer.data.command.util.CommandError.*
import de.martenschaefer.data.test.UnitSpec
import de.martenschaefer.data.util.DataResult.*

class CommandTest extends UnitSpec {
    val command: Command[String] = Command.build {
        literal("do") {
            literal("something") {
                result("Did something")
            }

            literal("nothing") {
                result("Did nothing")
            }
        }

        literal("test") {
            literal("nothing") {
                result("Didn't test anything", false)
            }

            argument(Argument.string("test name")) { testName =>
                result(s"Tested $testName")
            }
        }
    }

    "a command" should "work correctly with correct input (1)" in {
        command.run(List("do", "something")) shouldBe Success("Did something")
    }

    it should "work correctly with correct input (2)" in {
        command.run(List("do", "nothing")) shouldBe Success("Did nothing")
    }

    it should "work correctly with correct input (3)" in {
        command.run(List("test", "nothing")) shouldBe Success("Didn't test anything")
    }

    it should "work correctly with correct input (4)" in {
        command.run(List("test", "commands")) shouldBe Success("Tested commands")
    }

    it should "fail with incorrect input (1)" in {
        command.run(List("do", "whatever")) shouldBe Failure(List(NoMatchingSubcommandsError(List("do", "whatever"), List(
            NoMatchingSubcommandsError(List("whatever"), List(
                ArgumentNotMatchedError(List("whatever"), literalArg("something").name),
                ArgumentNotMatchedError(List("whatever"), literalArg("nothing").name)
            )),
            ArgumentNotMatchedError(List("do", "whatever"), literalArg("test").name)
        ))))
    }

    it should "fail with incorrect input (2)" in {
        command.run(List("do")) shouldBe Failure(List(NoMatchingSubcommandsError(List("do"), List(
            NoMatchingSubcommandsError(List.empty, List(
                ArgumentNotMatchedError(List.empty, literalArg("something").name),
                ArgumentNotMatchedError(List.empty, literalArg("nothing").name)
            )),
            ArgumentNotMatchedError(List("do"), literalArg("test").name)
        ))))
    }

    it should "fail with incorrect input (3)" in {
        command.run(List("test")) shouldBe Failure(List(NoMatchingSubcommandsError(List("test"), List(
            NoMatchingSubcommandsError(List.empty, List(
                ArgumentNotMatchedError(List.empty, literalArg("nothing").name),
                ArgumentNotMatchedError(List.empty, string("test name").name)
            )),
            ArgumentNotMatchedError(List("test"), literalArg("do").name)
        ))))
    }

    it should "fail with incorrect input (4)" in {
        command.run(List("test", "nothing", "else")) shouldBe Success("Tested nothing")
    }
}
