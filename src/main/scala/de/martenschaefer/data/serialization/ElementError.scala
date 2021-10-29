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
     * Returns this element error, but with the passed function applied on the path.
     *
     * @param f The function to apply on the path
     * @return The new element error
     */
    def mapPath(f: List[ElementNode] => List[ElementNode]): ElementError

    /**
     * Returns this element error with the given node prepended to the path.
     *
     * @param prependedPath The {@link ElementNode} to prepend.
     * @return The new element error
     */
    def withPrependedPath(prependedPath: ElementNode): ElementError =
        this.mapPath(prependedPath :: _)

    /**
     * Returns this element error with the an {@code ElementNode.Name} prepended to the path.
     *
     * @param prependedPath The path node to prepend.
     * @return The new element error
     */
    def withPrependedPath(prependedPath: String): ElementError = this.withPrependedPath(ElementNode.Name(prependedPath))

    /**
     * @return a list of sub errors.
     */
    def getSubErrors: List[ElementError] = List.empty
}

object ElementError {
    /**
     * @param path The path to the error
     * @return the path as a {@code String}.
     */
    def getPath(path: List[ElementNode]): String =
        if (path.isEmpty) "root node" else path.mkString("", "", "").tail
}

case class ValidationError(val message: String => String, override val path: List[ElementNode] = List.empty) extends ElementError(path) {
    override def getDescription(path: String): String = this.message(path)

    override def mapPath(f: List[ElementNode] => List[ElementNode]): ElementError =
        ValidationError(this.message, f(this.path))
}

case class ParseError(val message: String, override val path: List[ElementNode]) extends ElementError(path) {
    override def getDescription(path: String): String = s"Parse error at $path: $message"

    override def mapPath(f: List[ElementNode] => List[ElementNode]): ElementError =
        ParseError(this.message, f(this.path))
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

    override def mapPath(f: List[ElementNode] => List[ElementNode]): ElementError = this match {
        case NotAnInt(e, path) => NotAnInt(e, f(path))
        case NotALong(e, path) => NotALong(e, f(path))
        case NotAFloat(e, path) => NotAFloat(e, f(path))
        case NotADouble(e, path) => NotADouble(e, f(path))
        case NotABoolean(e, path) => NotABoolean(e, f(path))
        case NotAString(e, path) => NotAString(e, f(path))
        case NotAnArray(e, path) => NotAnArray(e, f(path))
        case NotAnObject(e, path) => NotAnObject(e, f(path))
        case MissingKey(e, path) => MissingKey(e, f(path))
        case ValidationParseError(msg, e, path) => ValidationParseError(msg, e, f(path))
        case EitherParseError(msg, e, path) => EitherParseError(msg, e, f(path))
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
        case MissingKey(_, _) => s"Missing key \"${this.path.last.toString.tail}\" in "
            + (if (this.path.size < 2) "root node" else this.path.dropRight(1).mkString("", "", "").tail)
        case ValidationParseError(msg, _, _) => msg(path)
        case EitherParseError(msg, _, _) => msg(path)
    }

    override def getDescription: String = super.getDescription + s": $element"
}

@deprecated
case class EitherError(val message: String => String) extends ElementError(List.empty) {
    override def getDescription(path: String): String = this.message(path)

    override def mapPath(f: List[ElementNode] => List[ElementNode]): ElementError =
        RecordParseError.EitherParseError(this.message, Element.None, f(List.empty))
}

@deprecated
object EitherError {
    def apply(using e: EitherError): EitherError = e

    def message(using e: EitherError): String => String = e.message
}

case class AlternativeError(val subErrors: List[AlternativeError.AlternativeSubError],
                            override val path: List[ElementNode]) extends ElementError(path) {

    override def getDescription(path: String): String = "Multiple alternatives failed:"

    override def mapPath(f: List[ElementNode] => List[ElementNode]): ElementError =
        AlternativeError(this.subErrors.map(_.mapPath(f)), f(this.path))

    override def getSubErrors: List[ElementError] = this.subErrors
}

object AlternativeError {
    def of(subErrors: List[List[ElementError]], path: List[ElementNode]): AlternativeError = {
        new AlternativeError(subErrors.zipWithIndex.map((errors, index) => AlternativeError.AlternativeSubError(index + 1, errors)), path)
    }

    case class AlternativeSubError(val index: Int, val errors: List[ElementError]) extends ElementError(List.empty) {
        override def getDescription(path: String): String =
            s"Alternative ${this.index}:"

        override def mapPath(f: List[ElementNode] => List[ElementNode]): AlternativeSubError =
            AlternativeSubError(this.index, this.errors.map(_.mapPath(f)))

        override def getSubErrors: List[ElementError] = this.errors
    }
}

case class NullElementError(override val path: List[ElementNode]) extends ElementError(path) {
    override def getDescription(path: String): String = s"$path is null"

    override def mapPath(f: List[ElementNode] => List[ElementNode]): ElementError =
        NullElementError(f(this.path))
}
