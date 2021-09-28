package de.martenschaefer.data.command.util

import de.martenschaefer.data.command.Command
import de.martenschaefer.data.command.argument.CommandArgument
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

    def forArgument[A, T](argument: CommandArgument[A], command: List[String])(f: A => T): Option[T] = {
        val nextCommand = if (command.isEmpty) "" else command(0)

        runFor(argument.get(nextCommand))(f)
    }

    def getSuggestionsForMatchingArgument[T, A](command: List[String],
                                                argument: CommandArgument[A],
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

    def hasFlag(command: List[String], flag: CommandArgument[Unit]): Boolean = {
        for (commandPart <- command)
            if (flag.get(commandPart).isDefined)
                return true
        false
    }

    def hasFlags[K](command: List[String], flags: Map[K, CommandArgument[Unit]]): Map[K, Boolean] = {
        var hasFlags: Map[K, Boolean] = Map.empty.default(false)

        for (commandPart <- command)
            flags.foreach((k, argument) => if (argument.get(commandPart).isDefined)
                hasFlags = hasFlags.updated(k, true))

        return hasFlags
    }

    def getArgumentFlagResult[A](argument: CommandArgument[A], flagArgument: CommandArgument[Unit], command: List[String]): Option[A] = {
        for (i <- 0 until command.length) {
            val commandPart = command(i)

            val commandPartParts = commandPart.split("=")

            if (commandPartParts.length > 0 && commandPartParts.length <= 2) {
                val flagPart = commandPartParts(0)

                if (flagArgument.get(flagPart).isDefined)
                    if (commandPartParts.length == 2) {
                        return argument.get(commandPartParts(1))
                    } else if (i < command.length - 1) {
                        return argument.get(command(i + 1))
                    }
            }
        }

        None
    }
}
