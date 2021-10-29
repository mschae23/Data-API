package de.martenschaefer.data.lang

import scala.collection.immutable.ListMap
import scala.collection.mutable.ListBuffer
import de.martenschaefer.data.serialization.{ ElementError, ElementNode }
import de.martenschaefer.data.util.Utils

case object PrimitiveParselet extends PrefixParselet {
    override def parse(parser: LangParser, token: LangToken): LangParser.Result = {
        token match {
            case LangToken.FunctionStart(name) =>
                Right(name.toIntOption match {
                    case Some(number) => LangExpression.IntLiteral(number)
                    case None => name.toLongOption match {
                        case Some(number) => LangExpression.LongLiteral(number)
                        case None => name.toDoubleOption match {
                            case Some(number) => LangExpression.DoubleLiteral(number)
                            case None => name.toBooleanOption match {
                                case Some(booleanValue) => LangExpression.BooleanLiteral(booleanValue)
                                case None =>
                                    if ("null".equals(name.strip)) LangExpression.Null

                                    else LangExpression.SymbolExpression(name)
                            }
                        }
                    }
                })

            case LangToken.StringToken(string) => Right(LangExpression.StringLiteral(string))

            case _ => Left(List(ParseletOnWrongTokenError(token)))
        }
    }
}

case class PrefixOperatorParselet(val precedence: Int = DefaultPrecedence.PREFIX) extends PrefixParselet {
    override def parse(parser: LangParser, token: LangToken): LangParser.Result = {
        token match {
            case LangToken.FunctionStart(operatorName) =>
                val operand = parser.parseExpression(this.precedence) match {
                    case Right(expression) => expression
                    case result: Left[_, _] => return result
                }

                Right(LangExpression.FunctionCall(operatorName, List(operand)))

            case _ => Left(List(ParseletOnWrongTokenError(token)))
        }
    }
}

case class GroupParselet(val precedence: Int = DefaultPrecedence.GROUP) extends PrefixParselet {
    override def parse(parser: LangParser, token: LangToken): LangParser.Result = {
        token match {
            case LangToken.ParenthesesOpen =>
                val expression = parser.parseExpression(this.precedence) match {
                    case Right(expression) => expression
                    case result: Left[_, _] => return result
                }

                parser.expect(LangToken.ParenthesesClose) match {
                    case Left(errors) => return Left(errors)
                    case _ =>
                }

                Right(expression)

            case _ => Left(List(ParseletOnWrongTokenError(token)))
        }
    }
}

case class ArrayParselet(val precedence: Int = DefaultPrecedence.OBJECT) extends PrefixParselet {
    override def parse(parser: LangParser, token: LangToken): LangParser.Result = {
        token match {
            case LangToken.ArrayStart =>
                val values = new ListBuffer[LangExpression]()

                var token = parser.peek match {
                    case Some(t) => t
                    case None => return Left(List(LangParser.UnexpectedEofError()))
                }

                while (token != LangToken.ArrayEnd) {
                    val expression = parser.parseExpression(this.precedence) match {
                        case Right(expr) => expr
                        case result: Left[_, _] => return result
                    }

                    values.addOne(expression)

                    val _ = parser.expect(LangToken.FieldEnd)

                    token = parser.peek match {
                        case Some(t) => t
                        case None => return Left(List(LangParser.UnexpectedEofError()))
                    }
                }

                parser.next()

                Right(LangExpression.ArrayLiteral(values.toList))

            case _ => Left(List(ParseletOnWrongTokenError(token)))
        }
    }
}

case class ObjectParselet(val precedence: Int = DefaultPrecedence.OBJECT) extends PrefixParselet {
    override def parse(parser: LangParser, token: LangToken): LangParser.Result = {
        token match {
            case LangToken.ObjectStart =>
                var fields = new ListMap[LangExpression, LangExpression]()

                var token = parser.peek match {
                    case Some(t) => t
                    case None => return Left(List(LangParser.UnexpectedEofError()))
                }

                while (token != LangToken.ObjectEnd) {
                    val key = parser.parseExpression(this.precedence) match {
                        case Right(expr) => expr
                        case result: Left[_, _] => return result
                    }

                    parser.expect(LangToken.FunctionStart(":")) match {
                        case Left(errors) => return Left(errors)
                        case _ =>
                    }

                    val value = parser.parseExpression(this.precedence) match {
                        case Right(expr) => expr
                        case result: Left[_, _] => return result
                    }

                    fields = fields.updated(key, value)

                    val _ = parser.expect(LangToken.FieldEnd)

                    token = parser.peek match {
                        case Some(t) => t
                        case None => return Left(List(LangParser.UnexpectedEofError()))
                    }
                }

                parser.next()

                Right(LangExpression.ObjectLiteral(fields))

            case _ => Left(List(ParseletOnWrongTokenError(token)))
        }
    }
}

case class BinaryOperatorParselet(override val precedence: Int, val rightAssociative: Boolean = false) extends Parselet {
    override def parse(parser: LangParser, left: LangExpression, token: LangToken): LangParser.Result = {
        token match {
            case LangToken.FunctionStart(operatorName) =>
                val operand = parser.parseExpression(if (this.rightAssociative) this.precedence - 1 else this.precedence) match {
                    case Right(expression) => expression
                    case result: Left[_, _] => return result
                }

                Right(LangExpression.FunctionCall(operatorName, List(left, operand)))

            case _ => Left(List(ParseletOnWrongTokenError(token)))
        }
    }
}

case class PostfixOperatorParselet(override val precedence: Int = DefaultPrecedence.POSTFIX) extends Parselet {
    override def parse(parser: LangParser, left: LangExpression, token: LangToken): LangParser.Result = {
        token match {
            case LangToken.FunctionStart(operatorName) =>
                Right(LangExpression.FunctionCall(operatorName, List(left)))

            case _ => Left(List(ParseletOnWrongTokenError(token)))
        }
    }
}

case class ObjectSyntaxFunctionCallParselet(override val precedence: Int = DefaultPrecedence.FUNCTION_CALL) extends Parselet {
    override def parse(parser: LangParser, left: LangExpression, token: LangToken): LangParser.Result = {
        token match {
            case LangToken.FunctionStart(".") =>
                val functionName = parser.parseExpression(this.precedence) match {
                    case Right(expression) => expression
                    case result: Left[_, _] => return result
                }

                val functionCall = FunctionCallParselet(this.precedence).parse(parser, functionName, parser.next() match {
                    case Some(token) => token
                    case None => return Left(List(LangParser.UnexpectedEofError()))
                }) match {
                    case Right(call) => call match {
                        case expr: LangExpression.FunctionCall => expr
                        case _ => return Left(List.empty)
                    }
                    case result: Left[_, _] => return result
                }

                Right(LangExpression.FunctionCall(functionCall.functionName, left :: functionCall.args))

            case _ => Left(List(ParseletOnWrongTokenError(token)))
        }
    }
}

case class FunctionCallParselet(override val precedence: Int = DefaultPrecedence.FUNCTION_CALL) extends Parselet {
    override def parse(parser: LangParser, left: LangExpression, token: LangToken): LangParser.Result = {
        left match {
            case LangExpression.SymbolExpression(name) => token match {
                case LangToken.ParenthesesOpen =>
                    val args = new ListBuffer[LangExpression]()

                    var token = parser.peek match {
                        case Some(t) => t
                        case None => return Left(List(LangParser.UnexpectedEofError()))
                    }

                    while (token != LangToken.ParenthesesClose) {
                        val expression = parser.parseExpression(this.precedence) match {
                            case Right(expr) => expr
                            case result: Left[_, _] => return result
                        }

                        args.addOne(expression)

                        val _ = parser.expect(LangToken.FieldEnd)

                        token = parser.peek match {
                            case Some(t) => t
                            case None => return Left(List(LangParser.UnexpectedEofError()))
                        }
                    }

                    parser.next()

                    Right(LangExpression.FunctionCall(name, args.toList))

                case _ => Left(List(ParseletOnWrongTokenError(token)))
            }

            case _ => Left(List(ExpectedSymbolError(left.toString)))
        }
    }
}

case class ParseletOnWrongTokenError(val token: LangToken, override val path: List[ElementNode] = List.empty) extends ElementError(path) {
    override def getDescription(path: String): String =
        s"Parselet called on wrong token: ${this.token}"

    override def mapPath(f: List[ElementNode] => List[ElementNode]): ElementError =
        ParseletOnWrongTokenError(this.token, f(this.path))
}

case class ExpectedSymbolError(val got: String, override val path: List[ElementNode] = List.empty) extends ElementError(path) {
    override def getDescription(path: String): String =
        s"Expected symbol, got ${this.got}"

    override def mapPath(f: List[ElementNode] => List[ElementNode]): ElementError =
        ExpectedSymbolError(this.got, f(this.path))
}
