package de.martenschaefer.data.test.command

import de.martenschaefer.data.command.Command
import de.martenschaefer.data.command.argument.CommandArgument
import de.martenschaefer.data.command.builder.CommandBuilder.*
import de.martenschaefer.data.test.UnitSpec

class ArgumentFlagsTest extends UnitSpec {
    val command: Command[String] = Command.build {
        literal("say") {
            argumentFlag("message", Some('m'), CommandArgument.string("message")) { message =>
                argumentFlag("person", Some('p'), CommandArgument.string("person")) { person =>
                    result(s"Said \"$message\" to $person.")
                }

                argumentFlag("to", Some('t'), CommandArgument.string("to")) { to =>
                    argumentFlag("number", Some('n'), CommandArgument.int("number")) { number =>
                        literal("literal") {
                            result(s"Said $message to $to ($number)")
                        }
                    }
                }
            }
        }
    }

    "Argument flags" should "work correctly with correct input (1)" in {
        command.run(List("say", "--message", "Hello!", "--person", "someone")) shouldBe Some("Said \"Hello!\" to someone.")
    }

    they should "work correctly with correct input (2)" in {
        command.run(List("say", "-m", "Hello!", "-p", "someone")) shouldBe Some("Said \"Hello!\" to someone.")
    }

    they should "work correctly with correct input (3)" in {
        command.run(List("say", "--message=Hello", "-p", "someone")) shouldBe Some("Said \"Hello\" to someone.")
    }

    they should "work correctly with correct input (4)" in {
        command.run(List("say", "-m=Hello", "-p", "someone")) shouldBe Some("Said \"Hello\" to someone.")
    }

    they should "work correctly with correct input (5)" in {
        command.run(List("say", "-mp", "Hello", "someone")) shouldBe Some("Said \"Hello\" to someone.")
    }

    they should "work correctly with correct input (6)" in {
        command.run(List("say", "-pm", "Hello", "someone")) shouldBe Some("Said \"Hello\" to someone.")
    }

    they should "work correctly with correct input (7)" in {
        command.run(List("say", "-p", "someone else", "-m", "hello")) shouldBe Some("Said \"hello\" to someone else.")
    }

    they should "work correctly with correct input (8)" in {
        command.run(List("say", "-m", "hello", "--to", "something", "-n=5", "literal")) shouldBe Some("Said hello to something (5)")
    }

    they should "work correctly with correct input (9)" in {
        command.run(List("say", "-mtn", "hello", "something", "5", "literal")) shouldBe Some("Said hello to something (5)")
    }

    they should "work correctly with correct input (10)" in {
        command.run(List("say", "-m", "hello", "--to", "something", "literal", "-n=5")) shouldBe Some("Said hello to something (5)")
    }

    they should "work correctly with correct input (11)" in {
        command.run(List("say", "-m", "hello", "literal", "--to", "something", "-n=5")) shouldBe Some("Said hello to something (5)")
    }

    they should "work correctly with correct input (12)" in {
        command.run(List("say", "literal", "-m=hello", "--to", "someone", "-n", "1")) shouldBe Some("Said hello to someone (1)")
    }

    they should "fail for incorrect input (1)" in {
        command.run(List("say", "--messag", "Hello!", "--person", "someone")) shouldBe None
    }

    they should "fail for incorrect input (2)" in {
        command.run(List("say", "--message", "Hello!", "--p", "someone")) shouldBe None
    }

    they should "fail for incorrect input (3)" in {
        command.run(List("say", "--message", "Hello!")) shouldBe None
    }

    they should "fail for incorrect input (4)" in {
        command.run(List("say", "--message", "Hello!", "-p")) shouldBe None
    }

    they should "fail for incorrect input (5)" in {
        command.run(List("say")) shouldBe None
    }

    they should "fail for incorrect input (6)" in {
        command.run(List("--message", "something", "say", "-t=someone", "n=9")) shouldBe None
    }
}
