package de.martenschaefer.data.serialization

trait ElementError(val path: List[ElementNode]) {
    /**
     * Returns a human-readable description of this error.
     *
     * @param path The path to the error.
     * @return The description of the error.
     */
    def getDescription(path: String): String

    /**
     * Returns a human-readable description of this error.
     *
     * @return The error description
     */
    def getDescription: String = this.getDescription(ElementError.getPath(this.path))

    override def toString: String = this.getDescription

    /**
     * Returns this element error with the given node prepended to the path.
     *
     * @param prependedPath The {@link ElementNode} to prepend.
     * @return The new element error
     */
    def withPrependedPath(prependedPath: ElementNode): ElementError

    /**
     * Returns this element error with the an {@code ElementNode.Name} prepended to the path.
     *
     * @param prependedPath The path node to prepend.
     * @return The new element error
     */
    def withPrependedPath(prependedPath: String): ElementError = this.withPrependedPath(ElementNode.Name(prependedPath))
}

object ElementError {
    /**
     * @param path The path to the error
     * @return the path as a {@code String}.
     */
    def getPath(path: List[ElementNode]): String =
        if (path.isEmpty) "root node" else path.mkString("", "", "").tail
}

case class ValidationError(val message: String => String, override val path: List[ElementNode]) extends ElementError(path) {
    override def getDescription(path: String): String = this.message(path)

    override def withPrependedPath(prependedPath: ElementNode): ElementError =
        ValidationError(this.message, prependedPath :: this.path)
}

case class ParseError(val message: String, override val path: List[ElementNode]) extends ElementError(path) {
    override def getDescription(path: String): String = s"Parse error at $path: $message"

    override def withPrependedPath(prependedPath: ElementNode): ElementError =
        ParseError(this.message, prependedPath :: this.path)
}

enum RecordParseError(val element: Element, override val path: List[ElementNode]) extends ElementError(path) {
    case NotAnInt(override val element: Element, override val path: List[ElementNode]) extends RecordParseError(element, path)
    case NotALong(override val element: Element, override val path: List[ElementNode]) extends RecordParseError(element, path)
    case NotAFloat(override val element: Element, override val path: List[ElementNode]) extends RecordParseError(element, path)
    case NotADouble(override val element: Element, override val path: List[ElementNode]) extends RecordParseError(element, path)
    case NotABoolean(override val element: Element, override val path: List[ElementNode]) extends RecordParseError(element, path)
    case NotAString(override val element: Element, override val path: List[ElementNode]) extends RecordParseError(element, path)
    case NotAnArray(override val element: Element, override val path: List[ElementNode]) extends RecordParseError(element, path)
    case NotAnObject(override val element: Element, override val path: List[ElementNode]) extends RecordParseError(element, path)
    case MissingKey(override val element: Element, override val path: List[ElementNode]) extends RecordParseError(element, path)

    case ValidationParseError(val message: String => String, override val element: Element, override val path: List[ElementNode])
      extends RecordParseError(element, path)

    case EitherParseError(val message: String => String, override val element: Element, override val path: List[ElementNode])
        extends RecordParseError(element, path)

    override def withPrependedPath(prependedPath: ElementNode): ElementError = this match {
        case NotAnInt(e, path) => NotAnInt(e, prependedPath :: path)
        case NotALong(e, path) => NotALong(e, prependedPath :: path)
        case NotAFloat(e, path) => NotAFloat(e, prependedPath :: path)
        case NotADouble(e, path) => NotADouble(e, prependedPath :: path)
        case NotABoolean(e, path) => NotABoolean(e, prependedPath :: path)
        case NotAString(e, path) => NotAString(e, prependedPath :: path)
        case NotAnArray(e, path) => NotAnArray(e, prependedPath :: path)
        case NotAnObject(e, path) => NotAnObject(e, prependedPath :: path)
        case MissingKey(e, path) => MissingKey(e, prependedPath :: path)
        case ValidationParseError(msg, e, path) => ValidationParseError(msg, e, prependedPath :: path)
        case EitherParseError(msg, e, path) => EitherParseError(msg, e, prependedPath :: path)
    }

    /**
     * @param path The path to the error.
     * @return A human-readable description of the error.
     */
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
        case ValidationParseError(msg, _, _) => msg(path)
        case EitherParseError(msg, _, _) => msg(path)
    }

    override def getDescription: String = super.getDescription + s": $element"
}

case class EitherError(val message: String => String) extends ElementError(List()) {
    override def getDescription(path: String): String = this.message(path)

    override def withPrependedPath(prependedPath: ElementNode): ElementError =
        RecordParseError.EitherParseError(this.message, Element.None, List(prependedPath))
}

object EitherError {
    def apply(using e: EitherError): EitherError = e

    def message(using e: EitherError): String => String = e.message
}
