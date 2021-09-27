package de.martenschaefer.data.test.command

import de.martenschaefer.data.command.Command
import de.martenschaefer.data.command.argument.CommandArgument
import de.martenschaefer.data.command.builder.CommandBuilder.*
import de.martenschaefer.data.test.UnitSpec

class CommandSuggestionsTest extends UnitSpec {
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

            argument(CommandArgument.string("test name")) { testName =>
                result(s"Tested $testName")
            }
        }
    }

    "command suggestions" should "return the last command part for correct input (1)" in {
        command.getSuggestions(List("do", "something")) shouldBe List("something")
    }

    they should "return the last command part for correct input (2)" in {
        command.getSuggestions(List("do", "nothing")) shouldBe List("nothing")
    }

    they should "return the last command part for correct input (3)" in {
        command.getSuggestions(List("test", "nothing")) shouldBe List("nothing")
    }

    they should "return the last command part for correct input (4)" in {
        command.getSuggestions(List("test", "commands")) shouldBe List("commands")
    }

    they should "give correct suggestions for incomplete input (1)" in {
        command.getSuggestions(List("do", "someth")) shouldBe List("something")
    }

    they should "give correct suggestions for incomplete input (2)" in {
        command.getSuggestions(List("do", "not")) shouldBe List("nothing")
    }

    they should "give correct suggestions for incomplete input (3)" in {
        command.getSuggestions(List("test")) shouldBe List("test")
    }

    they should "give correct suggestions for incomplete input (4)" in {
        command.getSuggestions(List("test", "")) shouldBe List("nothing")
    }

    they should "return an empty list for incorrect input (1)" in {
        command.getSuggestions(List("do", "whatever")) shouldBe List.empty
    }

    they should "return an empty list for incorrect input (2)" in {
        command.getSuggestions(List("something")) shouldBe List.empty
    }

    they should "return an empty list for incorrect input (3)" in {
        command.getSuggestions(List("teste")) shouldBe List.empty
    }

    they should "return an empty list for incorrect input (4)" in {
        command.getSuggestions(List("test", "nothing", "else")) shouldBe List.empty
    }
}
