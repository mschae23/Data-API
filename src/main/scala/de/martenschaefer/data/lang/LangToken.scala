package de.martenschaefer.data.lang

enum LangToken {
    case ObjectStart, ObjectFieldDelimiter, ObjectEnd
    case ArrayStart, ArrayEnd
    case FieldEnd

    case StringToken(val string: String)

    case FunctionStart(val functionName: String)
    case ParenthesisOpen, ParenthesesClose
}
