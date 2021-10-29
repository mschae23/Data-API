package de.martenschaefer.data.lang

enum LangToken {
    case ObjectStart, ObjectFieldDelimiter, ObjectEnd
    case ArrayStart, ArrayEnd
    case FieldEnd

    case StringToken(val string: String)

    case FunctionName(val functionName: String)
    case ParenthesesOpen, ParenthesesClose

    case EndOfFile
}
