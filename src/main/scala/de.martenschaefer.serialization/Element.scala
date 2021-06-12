package de.martenschaefer.serialization

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
}
