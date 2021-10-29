package de.martenschaefer.data.lang

import de.martenschaefer.data.util.Utils

case object PrimitiveParselet extends PrefixParselet {
    override def parse(parser: LangParser, token: LangToken): Option[LangExpression] = {
        token match {
            case LangToken.FunctionStart(name) =>
                if (!parser.peek.contains(LangToken.ParenthesisOpen))
                    Some(name.toIntOption match {
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
                else
                    None

            case _ => None
        }
    }
}

case class PrefixOperatorParselet(val precedence: Int = DefaultPrecedence.PREFIX) extends PrefixParselet {
    override def parse(parser: LangParser, token: LangToken): Option[LangExpression] = {
        token match {
            case LangToken.FunctionStart(operatorName) =>
                val operand = parser.parseExpression(this.precedence) match {
                    case Right(expression) => expression
                    case Left(_) => return None
                }

                Some(LangExpression.FunctionCall(operatorName, Some(List(operand))))

            case _ => None
        }
    }
}

case class BinaryOperatorParselet(override val precedence: Int, val rightAssociative: Boolean = false) extends Parselet {
    override def parse(parser: LangParser, left: LangExpression, token: LangToken): Option[LangExpression] = {
        token match {
            case LangToken.FunctionStart(operatorName) =>
                val operand = parser.parseExpression(if (this.rightAssociative) this.precedence - 1 else this.precedence) match {
                    case Right(expression) => expression
                    case Left(_) => return None
                }

                Some(LangExpression.FunctionCall(operatorName, Some(List(left, operand))))

            case _ => None
        }
    }
}

case class PostfixOperatorParselet(val precedence: Int = DefaultPrecedence.POSTFIX) extends Parselet {
    override def parse(parser: LangParser, left: LangExpression, token: LangToken): Option[LangExpression] = {
        token match {
            case LangToken.FunctionStart(operatorName) =>
                Some(LangExpression.FunctionCall(operatorName, Some(List(left))))

            case _ => None
        }
    }
}
