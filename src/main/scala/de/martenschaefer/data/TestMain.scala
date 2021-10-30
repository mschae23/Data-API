package de.martenschaefer.data

import cats.data.Writer
import de.martenschaefer.data.command.Command
import de.martenschaefer.data.command.argument.CommandArgument as Argument
import de.martenschaefer.data.command.builder.CommandBuilder.*
import de.martenschaefer.data.lang.*
import de.martenschaefer.data.lang.LangExpression.*
import de.martenschaefer.data.registry.impl.SimpleRegistry
import de.martenschaefer.data.registry.{ Registry, RegistryKey }
import de.martenschaefer.data.serialization.Element.*
import de.martenschaefer.data.serialization.{ Codec, JsonCodecs, ValidationError }
import de.martenschaefer.data.util.{ DataResult, Identifier, Lifecycle }

object TestMain {
    def main(args: Array[String]): Unit = {
        val testString =
            """
              |{
              |    "test": 3,
              |    "test_2": {
              |        "test_operators": 3++ + 5 * 2
              |        test_function: feature(2, -4).apply({})
              |    },
              |}""".stripMargin

        val tokens = LangLexer.getTokens(testString) match {
            case Right(tokens) => tokens
            case Left(errors) => println(errors); return
        }
        println(tokens)
        println()

        val parser = new LangParser(tokens)
        parser.registerObjectParselets()
        parser.registerFunctionParselets()

        parser.registerPrefix("-", PrefixOperatorParselet())
        parser.register("+", BinaryOperatorParselet(DefaultPrecedence.SUM))
        parser.register("-", BinaryOperatorParselet(DefaultPrecedence.SUM))
        parser.register("*", BinaryOperatorParselet(DefaultPrecedence.PRODUCT))
        parser.register("/", BinaryOperatorParselet(DefaultPrecedence.PRODUCT))
        parser.register("++", PostfixOperatorParselet())
        parser.registerDefaultPrimitiveParselet()

        val expression: LangExpression = parser.getExpression() match {
            case Right(expr) => expr
            case Left(errors) =>
                println(errors)
                return
        }

        println(expression)
        println()

        val processor = new ExpressionProcessor(expression)
        processor.registerPostProcessor { expression =>
            expression match {
                case FunctionCall(function, IntLiteral(arg1) :: IntLiteral(arg2) :: Nil) => {
                    println(s"Infix function $function with args: $arg1, $arg2")
                    function match {
                        case LangExpression.SymbolExpression("+") => Writer.value(IntLiteral(arg1 + arg2))
                        case LangExpression.SymbolExpression("-") => Writer.value(IntLiteral(arg1 - arg2))
                        case LangExpression.SymbolExpression("*") => Writer.value(IntLiteral(arg1 * arg2))
                        case LangExpression.SymbolExpression("/") => Writer.value(IntLiteral(arg1 / arg2))
                        case _ => Writer(List(ValidationError(_ => s"Infix function $function not resolved")), expression)
                    }
                }

                case FunctionCall(function, IntLiteral(arg) :: Nil) => function match {
                    case LangExpression.SymbolExpression("-") => Writer.value(IntLiteral(-arg))
                    case _ => Writer.value(expression)
                }

                case _ => Writer.value(expression)
            }
        }

        val processedExpression = processor.process()
        println(processedExpression)
    }
}
