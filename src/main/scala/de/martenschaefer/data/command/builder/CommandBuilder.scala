package de.martenschaefer.data.command.builder

import scala.util.control.Breaks.break
import de.martenschaefer.data.command.Command
import de.martenschaefer.data.command.argument.CommandArgument

object CommandBuilder {
    private type Context[T] = CommandBuilderContext[T]
    private type Argument[T] = CommandArgument[T]
    type Function[T] = Context[T] ?=> Unit

    def build[T](builder: Function[T]): Command[T] = {
        val context = new Context[T](List.empty)
        builder(using context)

        val subCommands = context.subCommands.reverse

        new Command[T] {
            override def run(command: List[String]): Option[T] = {
                for (i <- 0 until subCommands.length) {
                    val subCommand = subCommands(i)

                    subCommand.run(command) match {
                        case Some(commandResult) => return Some(commandResult)
                        case _ =>
                    }
                }

                None
            }

            override def getSuggestions(command: List[String]): List[String] = {
                for (i <- 0 until subCommands.length) {
                    val subCommand = subCommands(i)

                    val suggestions = subCommand.getSuggestions(command)

                    if (!suggestions.isEmpty)
                        return suggestions
                }

                List.empty
            }
        }
    }

    def result[T](result: T, allowNonEmptyCommand: Boolean = true)(using context: Context[T]): Unit = {
        context.subCommands ::= createNextCommand(createResultCommand(result),
            command => if (allowNonEmptyCommand) List.empty else command)
    }

    def argument[T, A](argument: Argument[A])(builder: A => Function[T])(using context: Context[T]): Unit =
        context.subCommands ::= new Command[T] {
            override def run(command: List[String]): Option[T] = {
                if (command.length < 1)
                    return None;

                argument.get(command(0)) match {
                    case Some(value) => {
                        build(builder(value)).run(command.tail)
                    }

                    case _ => return None;
                }
            }

            override def getSuggestions(command: List[String]): List[String] = {
                if (command.length >= 1) {
                    argument.get(command(0)) match {
                        case Some(value) => {
                            if (command.tail.isEmpty)
                                return argument.getSuggestions(command(0)) match {
                                    case suggestions@head :: _ => suggestions

                                    case _ => List(command(0))
                                }
                            else
                                return build(builder(value)).getSuggestions(command.tail)
                        }

                        case _ =>
                    }
                }

                val argumentPart = if (command.isEmpty) "" else command(0)

                argument.getSuggestions(argumentPart)
            }
        }

    def optionalArgument[T, A](argument: Argument[A])(builder: Option[A] => Function[T])(using context: Context[T]): Unit =
        context.subCommands ::= new Command[T] {
            override def run(command: List[String]): Option[T] = {
                val argumentPart = if (command.isEmpty) "" else command(0)

                argument.get(argumentPart) match {
                    case Some(value) => {
                        build(builder(Some(value))).run(command.tail)
                    }

                    case _ =>
                        build(builder(None)).run(command.tail);
                }
            }

            override def getSuggestions(command: List[String]): List[String] = {
                val argumentPart = if (command.isEmpty) "" else command(0)

                argument.get(argumentPart) match {
                    case Some(value) => {
                        if (command.tail.isEmpty)
                            return argument.getSuggestions(command(0)) match {
                                case suggestions@head :: _ => suggestions

                                case _ => List(command(0))
                            }
                        else
                            return build(builder(Some(value))).getSuggestions(command.tail)
                    }

                    case _ =>
                }

                val suggestions = argument.getSuggestions(argumentPart)

                if (suggestions.isEmpty)
                    List("")
                else
                    suggestions
            }
        }

    def defaultedArgument[T, A](argument: Argument[A], alternative: => A)(builder: A => Function[T])(using context: Context[T]): Unit =
        optionalArgument(argument) { optionA =>
            builder(optionA.getOrElse(alternative))
        }

    def literal[T](literal: String)(builder: Function[T])(using context: Context[T]): Unit =
        argument[T, Unit](CommandArgument.literal(literal))(_ => builder)

    def withFlag[T](flag: String, shortFlag: Option[Char] = None)(builder: Boolean => Function[T])(using context: Context[T]): Unit =
        val argument = CommandArgument.flag(flag, shortFlag)

        context.subCommands ::= new Command[T] {
            override def run(command: List[String]): Option[T] = {
                var hasFlag = false

                for (commandPart <- command)
                    if (argument.get(commandPart).isDefined)
                        hasFlag = true

                build(builder(hasFlag)).run(command)
            }

            override def getSuggestions(command: List[String]): List[String] = {
                var hasFlag = false

                for (commandPart <- command)
                    if (argument.get(commandPart).isDefined)
                        hasFlag = true

                if (!hasFlag) {
                    val argumentPart = if (command.isEmpty) "" else command(0)

                    val suggestions = argument.getSuggestions(argumentPart)

                    if (!suggestions.isEmpty)
                        return suggestions
                }

                build(builder(hasFlag)).getSuggestions(command)
            }
        }

    def withFlags[T, K](flags: Map[K, Argument[Unit]])(builder: Map[K, Boolean] => Function[T])(using context: Context[T]): Unit =
        context.subCommands = new Command[T] {
            override def run(command: List[String]): Option[T] = {
                var hasFlags: Map[K, Boolean] = Map.empty

                for (commandPart <- command)
                    flags.foreach((k, argument) => if (argument.get(commandPart).isDefined)
                        hasFlags = hasFlags.updated(k, true))

                for ((k, argument) <- flags)
                    if (!hasFlags.contains(k))
                        hasFlags = hasFlags.updated(k, false)

                build(builder(hasFlags)).run(command)
            }

            override def getSuggestions(command: List[String]): List[String] = {
                var hasFlags: Map[K, Boolean] = Map.empty

                for (commandPart <- command)
                    flags.foreach((k, argument) => if (argument.get(commandPart).isDefined)
                        hasFlags = hasFlags.updated(k, true))

                for ((k, argument) <- flags)
                    if (!hasFlags.contains(k))
                        hasFlags = hasFlags.updated(k, false)

                val argumentPart = if (command.isEmpty) "" else command(0)

                val suggestions = flags.flatMap(_._2.getSuggestions(argumentPart))

                if (!suggestions.isEmpty)
                    return suggestions.toList

                build(builder(hasFlags)).getSuggestions(command)
            }
        } :: context.subCommands

    def literalFlag[T](flag: String, shortFlag: Option[Char] = None)(builder: Function[T])(using context: Context[T]): Unit = {
        val argument = CommandArgument.flag(flag, shortFlag)

        context.subCommands ::= new Command[T] {
            override def run(command: List[String]): Option[T] = {
                if (command.isEmpty)
                    return None;

                var hasFlag = false

                for (commandPart <- command)
                    if (argument.get(commandPart).isDefined)
                        hasFlag = true

                if (hasFlag)
                    return build(builder).run(command)

                None
            }

            override def getSuggestions(command: List[String]): List[String] = {
                var hasFlag = false

                for (commandPart <- command)
                    if (argument.get(commandPart).isDefined)
                        hasFlag = true

                if (!hasFlag) {
                    val argumentPart = if (command.isEmpty) "" else command(0)

                    return argument.getSuggestions(argumentPart)
                }

                build(builder).getSuggestions(command)
            }
        }
    }

    def argumentFlag[T, A](flag: String, shortFlag: Option[Char] = None, argument: Argument[A])(
        builder: A => Function[T])(using context: Context[T]): Unit = {
        val flagArgument = CommandArgument.flag(flag, shortFlag)

        def getResultArgument(command: List[String]): Option[A] = {
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

        context.subCommands ::= new Command[T] {
            override def run(command: List[String]): Option[T] = {
                if (command.length < 1)
                    return None;

                val resultArgument = getResultArgument(command)

                for (result <- resultArgument)
                    return build(builder(result)).run(command)

                None
            }

            override def getSuggestions(command: List[String]): List[String] = {
                for (i <- 0 until command.length) {
                    val commandPart = command(i)

                    val commandPartParts = commandPart.split("=")

                    if (commandPartParts.length > 0 && commandPartParts.length <= 2) {
                        val flagPart = commandPartParts(0)

                        if (flagArgument.get(flagPart).isDefined) {
                            if (commandPartParts.length == 2) {
                                return argument.getSuggestions(commandPartParts(1))
                            } else if (i < command.length - 1) {
                                return argument.getSuggestions(command(i + 1))
                            }
                        } else
                            return flagArgument.getSuggestions(flagPart)
                    }
                }

                val resultArgument = getResultArgument(command)

                for (result <- resultArgument)
                    return build(builder(result)).getSuggestions(command)

                List.empty
            }
        }
    }

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
}
