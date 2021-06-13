package de.martenschaefer.data.serialization

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
        case Null => "null"
        case None => "none"

        case IntElement(value) => s"$value (int)"
        case LongElement(value) => s"$value (long)"
        case FloatElement(value) => s"$value (float)"
        case DoubleElement(value) => s"$value (double)"
        case BooleanElement(value) => s"$value (boolean)"
        case StringElement(value) => s"\"$value\""

        case ArrayElement(values) => values.mkString("[", ", ", "]")

        case ObjectElement(fields) => fields.mkString("Object(", ", ", ")")
    }
}
