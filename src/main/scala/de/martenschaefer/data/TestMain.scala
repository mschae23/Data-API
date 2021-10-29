package de.martenschaefer.data

import de.martenschaefer.data.command.Command
import de.martenschaefer.data.command.argument.CommandArgument as Argument
import de.martenschaefer.data.command.builder.CommandBuilder.*
import de.martenschaefer.data.lang.*
import de.martenschaefer.data.registry.impl.SimpleRegistry
import de.martenschaefer.data.registry.{ Registry, RegistryKey }
import de.martenschaefer.data.serialization.Element.*
import de.martenschaefer.data.serialization.{ Codec, JsonCodecs }
import de.martenschaefer.data.util.{ DataResult, Identifier, Lifecycle }

object TestMain {
    def main(args: Array[String]): Unit = {
        val testString =
            """
              |{
              |    "test": 3,
              |    "test_2": {
              |        test_operators: 3 + 5 * 2,
              |        "test_function": feature(2, 4).apply({})
              |    }
              |}""".stripMargin

        val testString2 = "3 + 5 * 2 - 9"

        val tokens = LangLexer.getTokens(testString2) match {
            case Right(tokens) => tokens
            case Left(errors) => println(errors); return
        }
        println(tokens)
        println()

        val parser = new LangParser(tokens)
        parser.registerPrefix("-", PrefixOperatorParselet())
        parser.registerPrefix(_ => true, PrimitiveParselet)
        parser.register("+", BinaryOperatorParselet(DefaultPrecedence.SUM))
        parser.register("-", BinaryOperatorParselet(DefaultPrecedence.SUM))
        parser.register("*", BinaryOperatorParselet(DefaultPrecedence.PRODUCT))
        parser.register("/", BinaryOperatorParselet(DefaultPrecedence.PRODUCT))

        val expression: LangExpression = parser.getExpression() match {
            case Right(expr) => expr
            case Left(errors) =>
                println(errors)
                return
        }

        println(expression)
    }
}
