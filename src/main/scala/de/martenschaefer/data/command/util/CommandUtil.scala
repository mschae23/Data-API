package de.martenschaefer.data.command.util

import scala.annotation.tailrec
import de.martenschaefer.data.command.Command
import de.martenschaefer.data.command.argument.{ CommandArgument, FlagArgument }
import de.martenschaefer.data.command.builder.CommandBuilder.{ Argument, Context, Function, build }

object CommandUtil {
    def createNextCommand[T](command: Command[T], nextCommand: List[String] => List[String]): Command[T] = new Command[T] {
        override def run(commandParts: List[String]): Option[T] =
            command.run(nextCommand(commandParts))

        override def getSuggestions(commandParts: List[String]): List[String] =
            command.getSuggestions(nextCommand(commandParts))
    }

    def createResultCommand[T](result: T): Command[T] = new Command[T] {
        override def run(command: List[String]): Option[T] = {
            if (command.isEmpty) Some(result)
            else None
        }

        override def getSuggestions(command: List[String]): List[String] = List.empty
    }

    def runFor[A, T](option: Option[A])(f: A => T): Option[T] = option match {
        case Some(value) => Some(f(value))

        case None => None
    }

    def forArgument[A, T](argument: Argument[A], command: List[String])(f: A => T): Option[T] = {
        val nextCommand = if (command.isEmpty) "" else command(0)

        runFor(argument.get(nextCommand))(f)
    }

    def getSuggestionsForMatchingArgument[T, A](command: List[String],
                                                argument: Argument[A],
                                                value: A, builder: A => Function[T]): List[String] =
        if (!command.isEmpty && command.tail.isEmpty)
            argument.getSuggestions(command(0)) match {
                case suggestions@head :: _ => suggestions

                case _ => List(command(0))
            }
        else if (!command.isEmpty)
            build(builder(value)).getSuggestions(command.tail)
        else
            command

    def hasFlag(command: List[String], flag: Argument[Unit]): Boolean = {
        for (commandPart <- command)
            if (flag.get(commandPart).isDefined)
                return true
        false
    }

    def hasFlags[K](command: List[String], flags: Map[K, Argument[Unit]]): Map[K, Boolean] = {
        var hasFlags: Map[K, Boolean] = Map.empty.default(false)

        for (commandPart <- command)
            flags.foreach((k, argument) => if (argument.get(commandPart).isDefined)
                hasFlags = hasFlags.updated(k, true))

        return hasFlags
    }

    def removeFirstMatchingArgument[A](command: List[String], argument: Argument[A]): List[String] = {
        for (i <- 0 until command.length)
            if (argument.get(command(i)).isDefined)
                return command.patch(i, Nil, 1)

        command
    }

    def removeFlags[K](command: List[String], flags: Map[K, Argument[Unit]]): List[String] = {
        @tailrec
        def loop(command: List[String], flag: Argument[Unit], remainingFlags: List[(K, Argument[Unit])]): List[String] =
            remainingFlags match {
                case head :: tail => loop(removeFirstMatchingArgument(command, flag), head._2, tail)

                case _ => removeFirstMatchingArgument(command, flag)
            }

        val flagList = flags.toList

        if (flagList.isEmpty)
            return command

        loop(command, flagList.head._2, flagList.tail)
    }

    def getArgumentFlagResult[A](flagArgument: Argument[Unit], getter: String => Option[A], command: List[String]): Option[(A, Int, Int)] = {
        for (i <- 0 until command.length) {
            val commandPart = command(i)

            val commandPartParts = commandPart.split("=")

            if (commandPartParts.length > 0 && commandPartParts.length <= 2) {
                val flagPart = commandPartParts(0)

                if (flagArgument.get(flagPart).isDefined) {
                    if (commandPartParts.length == 2) {
                        return getter(commandPartParts(1)).map((_, i, 1))
                    } else if (i < command.length - 1) {
                        return getter(command(i + 1)).map((_, i, 2))
                    }
                }
            }
        }

        None
    }

    def getArgumentFlagSuggestions(flagArgument: Argument[Unit], getter: String => List[String],
                                   commandParts: List[String], command: List[String]): List[String] = {
        if (commandParts.length > 0 && commandParts.length <= 2) {
            val flagPart = commandParts(0)

            if (flagArgument.get(flagPart).isDefined) {
                if (commandParts.length == 2) {
                    return getter(commandParts(1))
                } else if (command.length > 1) {
                    return getter(command(1))
                }
            } else
                return flagArgument.getSuggestions(flagPart)
        }

        List.empty
    }
}
