package de.martenschaefer.data.test.command

import de.martenschaefer.data.command.Command
import de.martenschaefer.data.command.argument.CommandArgument as Argument
import de.martenschaefer.data.command.argument.CommandArgument.{ literal as literalArg, * }
import de.martenschaefer.data.command.builder.CommandBuilder.*
import de.martenschaefer.data.command.util.CommandError.*
import de.martenschaefer.data.test.UnitSpec
import de.martenschaefer.data.util.DataResult.*

class ArgumentFlagsTest extends UnitSpec {
    val command: Command[String] = Command.build {
        literal("say") {
            argumentFlag("message", Some('m'), Argument.string("message")) { message =>
                argumentFlag("person", Some('p'), Argument.string("person")) { person =>
                    result(s"Said \"$message\" to $person.")
                }

                argumentFlag("to", Some('t'), Argument.string("to")) { to =>
                    argumentFlag("number", Some('n'), Argument.int("number")) { number =>
                        literal("literal") {
                            result(s"Said $message to $to ($number)")
                        }
                    }
                }
            }
        }
    }

    "Argument flags" should "work correctly with correct input (1)" in {
        command.run(List("say", "--message", "Hello!", "--person", "someone")) shouldBe Success("Said \"Hello!\" to someone.")
    }

    they should "work correctly with correct input (2)" in {
        command.run(List("say", "-m", "Hello!", "-p", "someone")) shouldBe Success("Said \"Hello!\" to someone.")
    }

    they should "work correctly with correct input (3)" in {
        command.run(List("say", "--message=Hello", "-p", "someone")) shouldBe Success("Said \"Hello\" to someone.")
    }

    they should "work correctly with correct input (4)" in {
        command.run(List("say", "-m=Hello", "-p", "someone")) shouldBe Success("Said \"Hello\" to someone.")
    }

    they should "work correctly with correct input (5)" in {
        command.run(List("say", "-mp", "Hello", "someone")) shouldBe Success("Said \"Hello\" to someone.")
    }

    they should "work correctly with correct input (6)" in {
        command.run(List("say", "-pm", "Hello", "someone")) shouldBe Success("Said \"Hello\" to someone.")
    }

    they should "work correctly with correct input (7)" in {
        command.run(List("say", "-p", "someone else", "-m", "hello")) shouldBe Success("Said \"hello\" to someone else.")
    }

    they should "work correctly with correct input (8)" in {
        command.run(List("say", "-m", "hello", "--to", "something", "-n=5", "literal")) shouldBe Success("Said hello to something (5)")
    }

    they should "work correctly with correct input (9)" in {
        command.run(List("say", "-mtn", "hello", "something", "5", "literal")) shouldBe Success("Said hello to something (5)")
    }

    they should "work correctly with correct input (10)" in {
        command.run(List("say", "-m", "hello", "--to", "something", "literal", "-n=5")) shouldBe Success("Said hello to something (5)")
    }

    they should "work correctly with correct input (11)" in {
        command.run(List("say", "-m", "hello", "literal", "--to", "something", "-n=5")) shouldBe Success("Said hello to something (5)")
    }

    they should "work correctly with correct input (12)" in {
        command.run(List("say", "literal", "-m=hello", "--to", "someone", "-n", "1")) shouldBe Success("Said hello to someone (1)")
    }

    they should "fail for incorrect input (1)" in {
        command.run(List("say", "--messag", "Hello!", "--person", "someone")) shouldBe Failure(List(
            NoMatchingSubcommandsError(List("say", "--messag", "Hello!", "--person", "someone"), List(
                NoMatchingSubcommandsError(List("--messag", "Hello!", "--person", "someone"), List(
                    FlagArgumentNotFoundError(List("--messag", "Hello!", "--person", "someone"), "message", string("message").name)
                ))
            ))
        ))
    }

    they should "fail for incorrect input (2)" in {
        command.run(List("say", "--message", "Hello!", "--p", "someone")) shouldBe Failure(List(
            NoMatchingSubcommandsError(List("say", "--message", "Hello!", "--p", "someone"), List(
                NoMatchingSubcommandsError(List("--message", "Hello!", "--p", "someone"), List(
                    NoMatchingSubcommandsError(List("--p", "someone"), List(
                        FlagArgumentNotFoundError(List("--p", "someone"), "person", string("person").name),
                        FlagArgumentNotFoundError(List("--p", "someone"), "to", string("to").name)
                    ))
                ))
            ))
        ))
    }

    they should "fail for incorrect input (3)" in {
        command.run(List("say", "--message", "Hello!")) shouldBe Failure(List(
            NoMatchingSubcommandsError(List("say", "--message", "Hello!"), List(
                NoMatchingSubcommandsError(List("--message", "Hello!"), List(
                    NoMatchingSubcommandsError(List.empty, List(
                        FlagArgumentNotFoundError(List.empty, "person", string("person").name),
                        FlagArgumentNotFoundError(List.empty, "to", string("to").name)
                    ))
                ))
            ))
        ))
    }

    they should "fail for incorrect input (4)" in {
        command.run(List("say", "--message", "Hello!", "-p")) shouldBe Failure(List(
            NoMatchingSubcommandsError(List("say", "--message", "Hello!", "-p"), List(
                NoMatchingSubcommandsError(List("--message", "Hello!", "-p"), List(
                    NoMatchingSubcommandsError(List("-p"), List(
                        FlagArgumentNotFoundError(List("-p"), "person", string("person").name),
                        FlagArgumentNotFoundError(List("-p"), "to", string("to").name)
                    ))
                ))
            ))
        ))
    }

    they should "fail for incorrect input (5)" in {
        command.run(List("say")) shouldBe Failure(List(NoMatchingSubcommandsError(List("say"), List(
            NoMatchingSubcommandsError(List.empty, List(
                FlagArgumentNotFoundError(List.empty, "message", string("message").name)
            ))
        ))))
    }

    they should "fail for incorrect input (6)" in {
        command.run(List("--message", "something", "say", "-t=someone", "n=9")) shouldBe Failure(List(
            NoMatchingSubcommandsError(List("--message", "something", "say", "-t=someone", "n=9"), List(
                ArgumentNotMatchedError(List("--message", "something", "say", "-t=someone", "n=9"), literalArg("say").name)
            ))
        ))
    }
}
