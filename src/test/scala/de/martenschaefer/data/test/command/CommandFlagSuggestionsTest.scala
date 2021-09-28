package de.martenschaefer.data.test.command

import de.martenschaefer.data.command.Command
import de.martenschaefer.data.command.argument.CommandArgument
import de.martenschaefer.data.command.builder.CommandBuilder.*
import de.martenschaefer.data.test.UnitSpec

class CommandFlagSuggestionsTest extends UnitSpec {
    val command: Command[String] = Command.build {
        literal("say") {
            argumentFlag("message", Some('m'), CommandArgument.string("message")) { message =>
                argumentFlag("receiver", Some('r'), CommandArgument.string("receiver")) { receiver =>
                    result(s"Said \"$message\" to $receiver.")
                }
            }

            withFlag("now") { now =>
                literal("something") {
                    if (now)
                        result("Said something now")
                    else
                        result("Said something")
                }

                literalFlag("nothing") {
                    if (now)
                        result("Said nothing now")
                    else
                        result("Said nothing")
                }

                literal("somewhere") {
                    result("Said \"somewhere\"")
                }
            }
        }
    }

    "command suggestions" should "return the last command part for correct input (1)" in {
        command.getSuggestions(List("say", "something")) shouldBe List("something")
    }

    they should "return the last command part for correct input (2)" in {
        command.getSuggestions(List("say", "something", "--now")) shouldBe List("--now")
    }

    they should "return the last command part for correct input (3)" in {
        command.getSuggestions(List("say", "--nothing")) shouldBe List("--nothing")
    }

    they should "return the last command part for correct input (4)" in {
        command.getSuggestions(List("say", "somewhere")) shouldBe List("somewhere")
    }

    they should "return the last command part for correct input (5)" in {
        command.getSuggestions(List("say", "--message")) shouldBe List("--message")
    }

    they should "give correct suggestions for incomplete input (1)" in {
        command.getSuggestions(List("say", "someth")) shouldBe List("something")
    }

    they should "give correct suggestions for incomplete input (2)" in {
        command.getSuggestions(List("say", "some")) shouldBe List("something", "somewhere")
    }

    they should "give correct suggestions for incomplete input (3)" in {
        command.getSuggestions(List("say", "--no")) shouldBe List("--nothing", "--now")
    }

    they should "give correct suggestions for incomplete input (4)" in {
        command.getSuggestions(List("say", "--nothing", "-")) shouldBe List("--now")
    }

    they should "give correct suggestions for incomplete input (5)" in {
        command.getSuggestions(List("say", "--me")) shouldBe List("--message")
    }

    they should "give correct suggestions for incomplete input (6)" in {
        command.getSuggestions(List("say", "-m", "yes", "--r")) shouldBe List("--receiver")
    }

    they should "give correct suggestions for incomplete input (7)" in {
        command.getSuggestions(List("say", "-m", "yes", "-")) shouldBe List("--receiver", "-r", "--now")
    }

    they should "return an empty list for incorrect input (1)" in {
        command.getSuggestions(List("say", "whatever")) shouldBe List.empty
    }

    they should "return an empty list for incorrect input (2)" in {
        command.getSuggestions(List("something")) shouldBe List.empty
    }

    they should "return an empty list for incorrect input (3)" in {
        command.getSuggestions(List("say", "--mass")) shouldBe List.empty
    }

    they should "return an empty list for incorrect input (4)" in {
        command.getSuggestions(List("say", "something", "else")) shouldBe List.empty
    }

    "Argument flags" should "return an empty list for correct input (1)" in {
        command.getSuggestions(List("say", "--message", "Hello!", "--receiver", "you")) shouldBe List.empty
    }

    they should "return an empty list for correct input (2)" in {
        command.getSuggestions(List("say", "--message", "Hello")) shouldBe List.empty
    }
}
