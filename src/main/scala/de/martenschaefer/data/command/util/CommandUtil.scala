package de.martenschaefer.data.command.util

import scala.annotation.tailrec
import de.martenschaefer.data.Result
import de.martenschaefer.data.command.Command
import de.martenschaefer.data.command.argument.{ CommandArgument, FlagArgument }
import de.martenschaefer.data.command.builder.CommandBuilder
import de.martenschaefer.data.command.builder.CommandBuilder.{ Argument, Context, Function }
import de.martenschaefer.data.util.DataResult.*

object CommandUtil {
    def createNextCommand[T](command: Command[T], nextCommand: List[String] => List[String]): Command[T] = new Command[T] {
        override def run(commandParts: List[String]): Result[T] =
            command.run(nextCommand(commandParts))

        override def getSuggestions(commandParts: List[String]): List[String] =
            command.getSuggestions(nextCommand(commandParts))
    }

    def createResultCommand[T](result: T): Command[T] = new Command[T] {
        override def run(command: List[String]): Result[T] = {
            if (command.isEmpty) Success(result)
            else Failure(List(CommandError.CommandNonEmptyForResultError(command)))
        }

        override def getSuggestions(command: List[String]): List[String] = List.empty
    }

    def forArgument[A, T](argument: Argument[A], command: List[String])(f: A => T): Result[T] = {
        val nextCommand = if (command.isEmpty) "" else command(0)

        argument.get(nextCommand) match {
            case Some(value) => Success(f(value))

            case _ => Failure(List(CommandError.ArgumentNotMatchedError(command, argument.name)))
        }
    }

    def getArgumentSuggestions[A, T](command: List[String], argument: Argument[A], builder: A => Function[T]): List[String] = {
        CommandUtil.forArgument(argument, command) { value =>
            return CommandUtil.getSuggestionsForMatchingArgument(command, argument, value, builder)
        }

        argument.getSuggestions(CommandUtil.getArgumentPart(command, 0))
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
            CommandBuilder.build(builder(value)).getSuggestions(command.tail)
        else
            command

    def getArgumentPart(command: List[String], i: Int): String = if (command.length <= i) "" else command(i)

    type PosResult[T] = (T, Int, Int)

    def hasArgument[A](command: List[String], flag: Argument[A]): PosResult[Boolean] = {
        for (i <- 0 until command.length)
            if (flag.get(command(i)).isDefined)
                return (true, i, 1)
        (false, 0, 0)
    }

    def hasFlags[K](command: List[String], flags: Map[K, Argument[Unit]]): Map[K, Boolean] = {
        var hasFlags: Map[K, Boolean] = Map.empty.default(false)

        for (commandPart <- command)
            flags.foreach((k, argument) => if (argument.get(commandPart).isDefined)
                hasFlags = hasFlags.updated(k, true))

        return hasFlags
    }

    def forFlagSuggestion(command: List[String],
                          hasFlag: Boolean, i: Int, used: Int, useLastPart: Boolean,
                          flag: Argument[Unit])(f: List[String] => List[String]): List[String] = {
        if (!hasFlag || command.drop(i + used).isEmpty) {
            f(flag.getSuggestions(CommandUtil.getArgumentPart(command, if (useLastPart) command.length - 1 else i)))
        } else
            List.empty
    }

    def getPatchedFlag(shortFlag: Option[Char], argument: String): List[String] =
        if (shortFlag.isDefined && argument.matches(FlagArgument.SHORT_FLAG_REGEX))
            List(argument.replaceFirst(shortFlag.get.toString, "").transform(replaced =>
                if ("-".equals(replaced)) return List.empty else replaced))
        else
            List.empty

    def removeFlag[A](command: List[String], argument: Argument[A], shortFlag: Option[Char]): List[String] = {
        for (i <- 0 until command.length) {
            val commandPart = command(i)

            if (argument.get(commandPart).isDefined)
                return command.patch(i, getPatchedFlag(shortFlag, commandPart), 1)
        }

        command
    }

    def removeFlags[K](command: List[String], flags: List[(Argument[Unit], Option[Char])]): List[String] = {
        @tailrec
        def loop(command: List[String], flag: Argument[Unit], shortFlag: Option[Char], remainingFlags: List[(Argument[Unit], Option[Char])]): List[String] =
            remainingFlags match {
                case head :: tail => loop(removeFlag(command, flag, shortFlag), head._1, head._2, tail)

                case _ => removeFlag(command, flag, shortFlag)
            }

        if (flags.isEmpty)
            return command

        loop(command, flags.head._1, flags.head._2, flags.tail)
    }

    def getArgumentFlagResult[A](flagArgument: Argument[Unit], getter: String => Option[A], command: List[String]): Option[PosResult[A]] = {
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

    def getArgumentFlag[T, A](command: List[String], flag: String, shortFlag: Option[Char], flagArgument: Argument[Unit], argument: Argument[A],
                              builder: A => Function[T]): Result[T] = {
        val resultArgument = CommandUtil.getArgumentFlagResult(flagArgument, argument.get(_), command)

        for ((result, i, used) <- resultArgument) {
            val patchedCommand = if (used == 2) {
                val patchedArgument = CommandUtil.getPatchedFlag(shortFlag, command(i))
                command.patch(i, patchedArgument, used)
            } else command.patch(i, Nil, used)

            return CommandBuilder.build(builder(result)).run(patchedCommand)
        }

        Failure(List(CommandError.FlagArgumentNotFoundError(command, flag, argument.name)))
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
                } else
                    return flagArgument.getSuggestions(flagPart)
            } else
                return flagArgument.getSuggestions(flagPart)
        }

        List.empty
    }

    def getArgumentFlagSuggestions[A](command: List[String], flagArgument: Argument[Unit], shortFlag: Option[Char], argument: Argument[A],
                                      suggestions: (Option[A], List[String]) => List[String]): List[String] = {
        val (hasFlag, i, used) = CommandUtil.hasArgument(command, flagArgument)

        val commandPart = if (command.isEmpty) "" else command(i)
        val commandParts = commandPart.split("=").toList

        val flagSuggestions =
            CommandUtil.getArgumentFlagSuggestions(flagArgument, argument.getSuggestions(_), commandParts, command)

        val resultArgument = CommandUtil.getArgumentFlagResult(flagArgument, argument.get(_), command)

        for ((result, i, used) <- resultArgument) {
            val patchedCommand = if (used == 2) {
                val patchedArgument = CommandUtil.getPatchedFlag(shortFlag, command(i))
                command.patch(i, patchedArgument, used)
            } else command.patch(i, Nil, used)

            return flagSuggestions ::: suggestions(Some(result), patchedCommand)
        }

        flagSuggestions ::: suggestions(None, command)
    }
}
