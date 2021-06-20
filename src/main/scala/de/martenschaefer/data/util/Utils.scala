package de.martenschaefer.data.util

import java.util.Locale
import de.martenschaefer.data.serialization.{ Element, ElementError, ElementNode, RecordParseError }

object Utils {
    def parsePrimitive(value: String): Element = value.toIntOption match {
        case Some(number) => Element.IntElement(number)
        case None => value.toLongOption match {
            case Some(number) => Element.LongElement(number)
            case None => value.toFloatOption match {
                case Some(number) => Element.FloatElement(number)
                case None => value.toDoubleOption match {
                    case Some(number) => Element.DoubleElement(number)
                    case None => value.toBooleanOption match {
                        case Some(booleanValue) => Element.BooleanElement(booleanValue)
                        case None =>
                            if ("null".equals(value.strip)) Element.Null
                            else Element.StringElement(value)
                    }
                }
            }
        }
    }

    def withPrependedPath(list: Vector[ElementError], prependedPath: ElementNode): Vector[ElementError] = list.map(_
        .withPrependedPath(prependedPath))

    def withPrependedPath(list: Vector[ElementError], prependedPath: String): Vector[ElementError] = withPrependedPath(
        list, ElementNode.Name(prependedPath))

    def toSnakeCase(string: String): String =
        string.flatMap(c => if (c.isUpper || c.isDigit) s"_${c.toLower}" else c.toString).dropWhile(_ == '_')
}
