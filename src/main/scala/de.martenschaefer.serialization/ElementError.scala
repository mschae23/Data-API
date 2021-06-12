package de.martenschaefer.serialization

enum ElementError(val element: Element, val path: List[String]) {
    case NotAnInt(override val element: Element, override val path: List[String]) extends ElementError(element, path)
    case NotALong(override val element: Element, override val path: List[String]) extends ElementError(element, path)
    case NotAFloat(override val element: Element, override val path: List[String]) extends ElementError(element, path)
    case NotADouble(override val element: Element, override val path: List[String]) extends ElementError(element, path)
    case NotABoolean(override val element: Element, override val path: List[String]) extends ElementError(element, path)
    case NotAString(override val element: Element, override val path: List[String]) extends ElementError(element, path)
    case NotAnArray(override val element: Element, override val path: List[String]) extends ElementError(element, path)
    case NotAnObject(override val element: Element, override val path: List[String]) extends ElementError(element, path)

    case Neither(override val element: Element, override val path: List[String]) extends ElementError(element, path)

    def withPrependedPath(prependedPath: String): ElementError = this match {
        case NotAnInt(e, path) => NotAnInt(e, prependedPath :: path)
        case NotALong(e, path) => NotALong(e, prependedPath :: path)
        case NotAFloat(e, path) => NotAFloat(e, prependedPath :: path)
        case NotADouble(e, path) => NotADouble(e, prependedPath :: path)
        case NotABoolean(e, path) => NotABoolean(e, prependedPath :: path)
        case NotAString(e, path) => NotAString(e, prependedPath :: path)
        case NotAnArray(e, path) => NotAnArray(e, prependedPath :: path)
        case NotAnObject(e, path) => NotAnObject(e, prependedPath :: path)
        case Neither(e, path) => Neither(e, prependedPath :: path)
    }

    def getDescription: String = this match {
        case NotAnInt(_, _) => "an int"
        case NotALong(_, _) => "a long"
        case NotAFloat(_, _) => "a float"
        case NotADouble(_, _) => "a double"
        case NotABoolean(_, _) => "a boolean"
        case NotAString(_, _) => "a String"
        case NotAnArray(_, _) => "an array"
        case NotAnObject(_, _) => "an object"
        case Neither(_, _) => "either something or something else"
    }

    def getNegativeDescription: String = this match {
        case NotAnInt(_, _) => "Not an int"
        case NotALong(_, _) => "Not a long"
        case NotAFloat(_, _) => "Not a float"
        case NotADouble(_, _) => "Not a double"
        case NotABoolean(_, _) => "Not a boolean"
        case NotAString(_, _) => "Not a String"
        case NotAnArray(_, _) => "Not an array"
        case NotAnObject(_, _) => "Not an object"
        case Neither(_, _) => "Doesn't fit either"
    }

    override def toString: String =
        (if (this.path.isEmpty) "" else this.path.mkString("", ".", ": ")) + this.getNegativeDescription + ": " + this.element
}
