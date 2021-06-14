package de.martenschaefer.data.serialization

enum ElementError(val element: Element, val path: List[ElementNode]) {
    case NotAnInt(override val element: Element, override val path: List[ElementNode]) extends ElementError(element, path)
    case NotALong(override val element: Element, override val path: List[ElementNode]) extends ElementError(element, path)
    case NotAFloat(override val element: Element, override val path: List[ElementNode]) extends ElementError(element, path)
    case NotADouble(override val element: Element, override val path: List[ElementNode]) extends ElementError(element, path)
    case NotABoolean(override val element: Element, override val path: List[ElementNode]) extends ElementError(element, path)
    case NotAString(override val element: Element, override val path: List[ElementNode]) extends ElementError(element, path)
    case NotAnArray(override val element: Element, override val path: List[ElementNode]) extends ElementError(element, path)
    case NotAnObject(override val element: Element, override val path: List[ElementNode]) extends ElementError(element, path)
    case MissingKey(override val element: Element, override val path: List[ElementNode]) extends ElementError(element, path)

    case ValidationError(val message: String => String, override val element: Element, override val path: List[ElementNode])
      extends ElementError(element, path)

    def withPrependedPath(prependedPath: ElementNode): ElementError = this match {
        case NotAnInt(e, path) => NotAnInt(e, prependedPath :: path)
        case NotALong(e, path) => NotALong(e, prependedPath :: path)
        case NotAFloat(e, path) => NotAFloat(e, prependedPath :: path)
        case NotADouble(e, path) => NotADouble(e, prependedPath :: path)
        case NotABoolean(e, path) => NotABoolean(e, prependedPath :: path)
        case NotAString(e, path) => NotAString(e, prependedPath :: path)
        case NotAnArray(e, path) => NotAnArray(e, prependedPath :: path)
        case NotAnObject(e, path) => NotAnObject(e, prependedPath :: path)
        case MissingKey(e, path) => MissingKey(e, prependedPath :: path)
        case ValidationError(msg, e, path) => ValidationError(msg, e, prependedPath :: path)
    }

    def withPrependedPath(prependedPath: String): ElementError = this.withPrependedPath(ElementNode.Name(prependedPath))

    def getDescription(path: String): String = this match {
        case NotAnInt(_, _) => s"$path is not an int"
        case NotALong(_, _) => s"$path is not a long"
        case NotAFloat(_, _) => s"$path is not a float"
        case NotADouble(_, _) => s"$path is not a double"
        case NotABoolean(_, _) => s"$path is not a boolean"
        case NotAString(_, _) => s"$path is not a String"
        case NotAnArray(_, _) => s"$path is not an array"
        case NotAnObject(_, _) => s"$path is not an object"
        case MissingKey(_, _) => s"Missing key \"${ this.path(this.path.size - 1).toString.tail }\" in "
            + (if (this.path.size < 2) "root node" else this.path.dropRight(1).mkString("", "", "").tail)
        case ValidationError(msg, _, _) => msg(path)
    }

    def getPath: String = this.path.mkString("", "", "").tail

    override def toString: String =
        s"${this.getDescription(if (this.path.isEmpty) "" else this.getPath)}: ${this.element}"
}
