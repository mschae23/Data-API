package de.martenschaefer.data.command.builder

import de.martenschaefer.data.command.Command
import de.martenschaefer.data.command.argument.CommandArgument

object CommandBuilder {
    private type Context[T] = CommandBuilderContext[T]
    private type Argument[T] = CommandArgument[T]

    def build[T](builder: Context[T] ?=> Unit): Command[T] = new Command[T] {
        override def run(command: List[String]): Option[T] = {
            val context = new Context[T](command, List.empty, List.empty)
            builder(using context)

            val fallbackNextCommand = if (command.isEmpty) command else command.tail

            val subCommands = context.subCommands.reverse
            val nextCommands = context.nextCommands.reverse

            for (i <- 0 until subCommands.length) {
                val subCommand = subCommands(i)
                val nextCommand = if (nextCommands.length > i) nextCommands(i) else fallbackNextCommand

                subCommand.run(nextCommand) match {
                    case Some(commandResult) => return Some(commandResult)
                    case _ =>
                }
            }

            None
        }
    }

    def result[T](result: T)(using context: Context[T]): Unit = {
        context.subCommands = createResultCommand(result) :: context.subCommands
        context.nextCommands = List.empty :: context.nextCommands
    }

    def argument[T, A](argument: Argument[A])(builder: A => Context[T] ?=> Unit)(using context: Context[T]): Unit = {
        if (context.command.length < 1)
            return;

        argument.get(context.command(0)) match {
            case Some(value) => {
                context.subCommands = build(builder(value)) :: context.subCommands
                context.nextCommands = context.command.tail :: context.nextCommands
            }

            case _ => return;
        }
    }

    def literal[T](literal: String)(builder: Context[T] ?=> Unit)(using context: Context[T]): Unit =
        argument[T, Unit](CommandArgument.literal(literal))(_ => builder)

    def withFlag[T](flag: String, shortFlag: Option[Char] = None)(builder: Boolean => Context[T] ?=> Unit)(using context: Context[T]): Unit = {
        val argument = CommandArgument.flag(flag, shortFlag)
        var hasFlag = false

        for (commandPart <- context.command)
            if (argument.get(commandPart).isDefined)
                hasFlag = true

        context.subCommands = build(builder(hasFlag)) :: context.subCommands
        context.nextCommands = context.command :: context.nextCommands
    }

    def flag[T](flag: String, shortFlag: Option[Char] = None)(builder: Context[T] ?=> Unit)(using context: Context[T]): Unit = {
        if (context.command.length < 1)
            return;

        val argument = CommandArgument.flag(flag, shortFlag)
        var hasFlag = false

        for (commandPart <- context.command)
            if (argument.get(commandPart).isDefined)
                hasFlag = true

        if (hasFlag) {
            context.subCommands = build(builder) :: context.subCommands
            context.nextCommands = context.command :: context.nextCommands
        }
    }

    def createResultCommand[T](result: T): Command[T] = new Command[T] {
        override def run(command: List[String]): Option[T] = command match {
            case head :: _ => None
            case _ => Some(result)
        }
    }
}
