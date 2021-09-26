package de.martenschaefer.data.command.builder

import scala.util.control.Breaks.break
import de.martenschaefer.data.command.Command
import de.martenschaefer.data.command.argument.CommandArgument

object CommandBuilder {
    private type Context[T] = CommandBuilderContext[T]
    private type Argument[T] = CommandArgument[T]

    def build[T](builder: Context[T] ?=> Unit): Command[T] = new Command[T] {
        override def run(command: List[String]): Option[T] = {
            val context = new Context[T](command, List.empty)
            builder(using context)

            val fallbackNextCommand = if (command.isEmpty) command else command.tail

            val subCommands = context.subCommands.reverse

            for (i <- 0 until subCommands.length) {
                val (subCommand, nextCommand) = subCommands(i)

                subCommand.run(nextCommand) match {
                    case Some(commandResult) => return Some(commandResult)
                    case _ =>
                }
            }

            None
        }
    }

    def result[T](result: T, allowNonEmptyCommand: Boolean = true)(using context: Context[T]): Unit = {
        context.subCommands ::= (createResultCommand(result), if (allowNonEmptyCommand) List.empty else context.command)
    }

    def argument[T, A](argument: Argument[A])(builder: A => Context[T] ?=> Unit)(using context: Context[T]): Unit = {
        if (context.command.length < 1)
            return;

        argument.get(context.command(0)) match {
            case Some(value) => {
                context.subCommands ::= (build(builder(value)), context.command.tail)
            }

            case _ => return;
        }
    }

    def optionalArgument[T, A](argument: Argument[A])(builder: Option[A] => Context[T] ?=> Unit)(using context: Context[T]): Unit = {
        argument.get(context.command(0)) match {
            case Some(value) => {
                context.subCommands ::= (build(builder(Some(value))), context.command.tail)
            }

            case _ => {
                context.subCommands ::= (build(builder(None)), context.command)
            }
        }
    }

    def defaultedArgument[T, A](argument: Argument[A], alternative: => A)(builder: A => Context[T] ?=> Unit)(using context: Context[T]): Unit =
        optionalArgument(argument) { optionA =>
            builder(optionA.getOrElse(alternative))
        }

    def literal[T](literal: String)(builder: Context[T] ?=> Unit)(using context: Context[T]): Unit =
        argument[T, Unit](CommandArgument.literal(literal))(_ => builder)

    def withFlag[T](flag: String, shortFlag: Option[Char] = None)(builder: Boolean => Context[T] ?=> Unit)(using context: Context[T]): Unit = {
        val argument = CommandArgument.flag(flag, shortFlag)
        var hasFlag = false

        for (commandPart <- context.command)
            if (argument.get(commandPart).isDefined)
                hasFlag = true

        context.subCommands ::= (build(builder(hasFlag)), context.command)
    }

    def withFlags[T, K](flags: Map[K, Argument[Unit]])(builder: Map[K, Boolean] => Context[T] ?=> Unit)(using context: Context[T]): Unit = {
        var hasFlags: Map[K, Boolean] = Map.empty

        for (commandPart <- context.command)
            flags.foreach((k, argument) => if (argument.get(commandPart).isDefined)
                hasFlags = hasFlags.updated(k, true))

        for ((k, argument) <- flags)
            if (!hasFlags.contains(k))
                hasFlags = hasFlags.updated(k, false)

        context.subCommands ::= (build(builder(hasFlags)), context.command)
    }

    def literalFlag[T](flag: String, shortFlag: Option[Char] = None)(builder: Context[T] ?=> Unit)(using context: Context[T]): Unit = {
        if (context.command.length < 1)
            return;

        val argument = CommandArgument.flag(flag, shortFlag)
        var hasFlag = false

        for (commandPart <- context.command)
            if (argument.get(commandPart).isDefined)
                hasFlag = true

        if (hasFlag) {
            context.subCommands ::= (build(builder), context.command)
        }
    }

    def argumentFlag[T, A](flag: String, shortFlag: Option[Char] = None, argument: Argument[A])(
        builder: A => Context[T] ?=> Unit)(using context: Context[T]): Unit = {
        if (context.command.length < 1)
            return;

        val flagArgument = CommandArgument.flag(flag, shortFlag)

        def getResultArgument(): Option[A] = {
            for (i <- 0 until context.command.length) {
                val commandPart = context.command(i)

                val commandPartParts = commandPart.split("=")

                if (commandPartParts.length > 0 && commandPartParts.length <= 2) {
                    val flagPart = commandPartParts(0)

                    if (flagArgument.get(flagPart).isDefined)
                        if (commandPartParts.length == 2) {
                            return argument.get(commandPartParts(1))
                        } else if (i < context.command.length - 1) {
                            return argument.get(context.command(i + 1))
                        }
                }
            }

            None
        }

        val resultArgument = getResultArgument()

        for (result <- resultArgument)
            context.subCommands ::= (build(builder(result)), context.command)
    }

    def createResultCommand[T](result: T): Command[T] = new Command[T] {
        override def run(command: List[String]): Option[T] = {
            if (command.isEmpty) Some(result)
            else None
        }
    }
}
