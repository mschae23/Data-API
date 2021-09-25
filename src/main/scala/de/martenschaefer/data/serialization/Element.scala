package de.martenschaefer.data.serialization

/**
 * {@code Element} is a data type that objects can be encoded to, or decoded from.
 * It can be nested with {@code ArrayElement} or {@code ObjectElement}.
 */
enum Element {
    case Null
    case None

    case IntElement(value: Int)
    case LongElement(value: Long)
    case FloatElement(value: Float)
    case DoubleElement(value: Double)
    case BooleanElement(value: Boolean)
    case StringElement(value: String)

    case ArrayElement(values: List[Element])

    case ObjectElement(fields: Map[String, Element])

    override def toString: String = this match {
        case Null | None => "null"

        case IntElement(value) => s"$value"
        case LongElement(value) => s"${value}L"
        case FloatElement(value) => s"${value}f"
        case DoubleElement(value) => s"$value"
        case BooleanElement(value) => s"$value"
        case StringElement(value) => s"\"$value\""

        case ArrayElement(values) => values.mkString("[", ", ", "]")

        case ObjectElement(fields) => fields.mkString("{ ", ", ", " }")
    }
}
