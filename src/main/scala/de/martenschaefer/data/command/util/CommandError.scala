package de.martenschaefer.data.command.util

import scala.annotation.tailrec
import de.martenschaefer.data.serialization.{ ElementError, ElementNode }

enum CommandError(val command: List[String]) extends ElementError(command.map(ElementNode.Name(_))) {
    case ArgumentNotMatchedError(override val command: List[String], val argument: String) extends CommandError(command)
    case FlagNotFoundError(override val command: List[String], val flag: String) extends CommandError(command)
    case FlagArgumentNotFoundError(override val command: List[String], val flag: String, val argument: String) extends CommandError(command)
    case CommandNonEmptyForResultError(override val command: List[String]) extends CommandError(command)
    case NoMatchingSubcommandsError(override val command: List[String], val errors: List[ElementError]) extends CommandError(command)

    def getDescriptions(path: String): List[String] = this match {
        case ArgumentNotMatchedError(_, argument) => List(s"Expected argument ($argument) at: $path")
        case FlagNotFoundError(_, flag) => List(s"Expected flag (\"--$flag\") in: $path")
        case FlagArgumentNotFoundError(_, flag, argument) => List(s"Expected flag argument (\"--$flag\" with argument: $argument) in: $path")
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
        case descriptions@_ :: _ => descriptions.mkString("{ ", ", ", " }")
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

    /* def getNoMatchingSubcommandsDepth: Int = {
        @tailrec
        def loop(error: ElementError, tail: List[ElementError], n: Int): Int = {
            tail match {
                case Nil => error match {
                    case NoMatchingSubcommandsError(command, errors) => errors match {
                        case Nil => return n + 1

                        case head :: tail => loop(head, tail, n + 1)
                    }

                    case _ => return n
                }

                case head :: tail => head match {
                    case NoMatchingSubcommandsError(command, errors) => errors match {
                        case Nil => tail match {
                            case Nil => return n + 1

                            case head2 :: tail2 => loop(head2, tail2, n)
                        }

                        case head2 :: tail2 => loop(head2, tail2, n + 1)
                    }

                    case _ => tail match {
                        case Nil => return n + 1

                        case head2 :: tail2 => loop(head2, tail2, n)
                    }
                }
            }
        }

        this match {
            case NoMatchingSubcommandsError(command, errors) => errors match {
                case Nil => return 1

                case head :: tail => loop(head, tail, 1)
            }

            case _ => return 0
        }
    } */
}

object CommandError {
    def flatten(errors: List[ElementError]): List[ElementError] = errors.flatMap(error => error match {
        case NoMatchingSubcommandsError(command, errors) => flatten(errors)

        case _ => List(error)
    })
}
