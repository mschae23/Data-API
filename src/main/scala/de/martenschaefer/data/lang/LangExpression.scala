package de.martenschaefer.data.lang

enum LangExpression {
    case Null
    case None

    case SymbolExpression(val symbol: String)

    case IntLiteral(val value: Int)
    case LongLiteral(val value: Long)
    case DoubleLiteral(val value: Double)
    case BooleanLiteral(val value: Boolean)
    case StringLiteral(val value: String)

    case ArrayLiteral(val values: List[LangExpression])
    case ObjectLiteral(val fields: Map[LangExpression, LangExpression])

    case FunctionCall(val functionName: String, val args: List[LangExpression])

    override def toString: String = this match {
        case Null | None => "null"

        case SymbolExpression(symbol) => symbol

        case IntLiteral(value) => s"$value"
        case LongLiteral(value) => s"${value}L"
        case DoubleLiteral(value) => s"$value"
        case BooleanLiteral(value) => s"$value"
        case StringLiteral(value) => s"\"$value\""

        case ArrayLiteral(values) => values.mkString("[", ", ", "]")
        case ObjectLiteral(fields) => fields.mkString("{ ", ", ", " }")

        case FunctionCall(functionName: String, args: List[LangExpression]) =>
            /* if (args.isDefined && args.get.length == 2 && !functionName.matches("^[a-zA-Z]*$"))
                s"${args.get.head} $functionName ${args.get(1)}"
            else */ s"$functionName${args.mkString("(", ", ", ")")}"
    }
}
