package de.martenschaefer.data.command.util

import de.martenschaefer.data.serialization.{ ElementError, ElementNode }

enum CommandError(val command: List[String]) extends ElementError(command.map(ElementNode.Name(_))) {
    case ArgumentNotMatchedError(override val command: List[String], val argument: String) extends CommandError(command)
    case FlagNotFoundError(override val command: List[String], val flag: String) extends CommandError(command)
    case FlagArgumentNotFoundError(override val command: List[String], val flag: String, val argument: String) extends CommandError(command)
    case CommandNonEmptyForResultError(override val command: List[String]) extends CommandError(command)

    case NoMatchingSubcommandsError(override val command: List[String], val errors: List[ElementError]) extends CommandError(command)

    def getDescriptions(path: String): List[String] = this match {
        case ArgumentNotMatchedError(_, argument) => List(s"Argument ($argument) not matched: $path")
        case FlagNotFoundError(_, flag) => List(s"Flag (\"--$flag\") not found: $path")
        case FlagArgumentNotFoundError(_, flag, argument) => List(s"Flag argument (\"--$flag\" with argument: $argument) not found: $path")
        case CommandNonEmptyForResultError(_) => List(s"Command non-empty for result: $path")

        case NoMatchingSubcommandsError(command, errors) => errors.flatMap { error =>
            if (error.isInstanceOf[CommandError])
                error.asInstanceOf[CommandError].getDescriptions
            else
                List(error.getDescription)
        }
    }

    def getDescriptions: List[String] = this.getDescriptions(getCommandString)

    override def getDescription(path: String): String = this.getDescriptions(path) match {
        case head :: Nil => head
        case descriptions @ _ :: _ => descriptions.mkString("{ ", ", ", " }")
        case Nil => s"No description found for error: $this"
    }

    override def getDescription: String = this.getDescription(getCommandString)

    override def toString: String = this match {
        case NoMatchingSubcommandsError(command, errors) =>
            errors.mkString(s"NoMatchingSubcommandsError($getCommandString, { ", ", ", " })")

        case _ => getDescription
    }

    def withPrependedPath(prependedPath: ElementNode): CommandError = {
        if (prependedPath.isInstanceOf[ElementNode.Index])
            throw new IllegalArgumentException(s"Command node can't be an index: $prependedPath")

        val node = prependedPath.asInstanceOf[ElementNode.Name].name

        this match {
            case ArgumentNotMatchedError(command, argument) => ArgumentNotMatchedError(node :: command, argument)
            case FlagNotFoundError(command, flag) => FlagNotFoundError(node :: command, flag)
            case FlagArgumentNotFoundError(command, flag, argument) => FlagArgumentNotFoundError(node :: command, flag, argument)
            case CommandNonEmptyForResultError(command) => CommandNonEmptyForResultError(node :: command)
            case NoMatchingSubcommandsError(command, errors) => NoMatchingSubcommandsError(node :: command, errors)
        }
    }

    def getCommandString: String =
        this.command.mkString("\"", "\" \"", "\"")

    def getNoMatchingSubcommandsDepth: Int = {
        def loop(error: ElementError): Int = error match {
            case NoMatchingSubcommandsError(_, errors) => errors.map(loop(_) + 1).max

            case _ => 0
        }

        loop(this)
    }
}
