package de.martenschaefer.data.test.command

import de.martenschaefer.data.command.Command
import de.martenschaefer.data.command.argument.CommandArgument as Argument
import de.martenschaefer.data.command.argument.CommandArgument.{ literal as literalArg, * }
import de.martenschaefer.data.command.builder.CommandBuilder.*
import de.martenschaefer.data.command.util.CommandError.*
import de.martenschaefer.data.test.UnitSpec
import de.martenschaefer.data.util.DataResult.*

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

            argumentFlag("test-name", Some('n'), Argument.string("test name")) { testName =>
                literal("now") {
                    result(s"Tested $testName now")
                }

                result(s"Tested $testName")
            }
        }
    }

    "a command" should "work correctly with correct input (1)" in {
        command.run(List("do", "something")) shouldBe Success("Did something")
    }

    it should "work correctly with correct input (2)" in {
        command.run(List("do", "something", "--good")) shouldBe Success("Did something well")
    }

    it should "work correctly with correct input (3)" in {
        command.run(List("do", "--good", "something")) shouldBe Success("Did something well")
    }

    it should "work correctly with correct input (4)" in {
        command.run(List("do", "--nothing")) shouldBe Success("Did nothing")
    }

    it should "work correctly with correct input (5)" in {
        command.run(List("test", "nothing")) shouldBe Success("Didn't test anything")
    }

    it should "work correctly with correct input (6)" in {
        command.run(List("test", "--test-name", "commands")) shouldBe Success("Tested commands")
    }

    it should "work correctly with correct input (7)" in {
        command.run(List("test", "--test-name=commands")) shouldBe Success("Tested commands")
    }

    it should "work correctly with correct input (8)" in {
        command.run(List("test", "-n", "commands")) shouldBe Success("Tested commands")
    }

    it should "work correctly with correct input (9)" in {
        command.run(List("test", "-n=commands")) shouldBe Success("Tested commands")
    }

    it should "work correctly with correct input (10)" in {
        command.run(List("test", "--test-name", "commands", "now")) shouldBe Success("Tested commands now")
    }

    it should "fail with incorrect input (1)" in {
        command.run(List("do", "whatever")) shouldBe Failure(List(NoMatchingSubcommandsError(List("do", "whatever"), List(
            NoMatchingSubcommandsError(List("whatever"), List(
                NoMatchingSubcommandsError(List("whatever"), List(
                    ArgumentNotMatchedError(List("whatever"), literalArg("something").name)
                )),
                FlagNotFoundError(List("whatever"), "nothing")
            )),
            ArgumentNotMatchedError(List("do", "whatever"), literalArg("test").name)
        ))))
    }

    it should "fail with incorrect input (2)" in {
        command.run(List("do", "nothing")) shouldBe Failure(List(
            NoMatchingSubcommandsError(List("do", "nothing"), List(
                NoMatchingSubcommandsError(List("nothing"), List(
                    NoMatchingSubcommandsError(List("nothing"), List(
                        ArgumentNotMatchedError(List("nothing"), literalArg("something").name)
                    )),
                    FlagNotFoundError(List("nothing"), "nothing")
                )),
                ArgumentNotMatchedError(List("do", "nothing"), literalArg("test").name)
            ))
        ))
    }

    it should "fail with incorrect input (3)" in {
        command.run(List("do")) shouldBe Failure(List(
            NoMatchingSubcommandsError(List("do"), List(
                NoMatchingSubcommandsError(List.empty, List(
                    NoMatchingSubcommandsError(List.empty, List(
                        ArgumentNotMatchedError(List.empty, literalArg("something").name)
                    )),
                    FlagNotFoundError(List.empty, "nothing")
                )),
                ArgumentNotMatchedError(List("do"), literalArg("test").name)
            ))
        ))
    }

    it should "fail with incorrect input (4)" in {
        command.run(List("test")) shouldBe Failure(List(
            NoMatchingSubcommandsError(List("test"), List(
                NoMatchingSubcommandsError(List.empty, List(
                    ArgumentNotMatchedError(List.empty, literalArg("nothing").name),
                    FlagArgumentNotFoundError(List.empty, "test-name", string("test name").name)
                )),
                ArgumentNotMatchedError(List("test"), literalArg("do").name)
            ))
        ))
    }

    it should "fail with incorrect input (5)" in {
        command.run(List("test", "something")) shouldBe Failure(List(
            NoMatchingSubcommandsError(List("test", "something"), List(
                NoMatchingSubcommandsError(List("something"), List(
                    ArgumentNotMatchedError(List("something"), literalArg("nothing").name),
                    FlagArgumentNotFoundError(List("something"), "test-name", string("test name").name)
                )),
                ArgumentNotMatchedError(List("test", "something"), literalArg("do").name)
            ))
        ))
    }
}
