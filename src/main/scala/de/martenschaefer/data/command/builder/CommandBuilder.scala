package de.martenschaefer.data.command.builder

import scala.util.control.Breaks.break
import de.martenschaefer.data.command.Command
import de.martenschaefer.data.command.argument.CommandArgument
import de.martenschaefer.data.command.util.CommandUtil

object CommandBuilder {
    type Context[T] = CommandBuilderContext[T]
    type Argument[T] = CommandArgument[T]
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
                var suggestions: List[String] = List.empty

                for (i <- 0 until subCommands.length) {
                    suggestions = suggestions ::: subCommands(i).getSuggestions(command)
                }

                suggestions.distinct.filter(!_.isBlank)
            }
        }
    }

    def result[T](result: T, allowNonEmptyCommand: Boolean = true)(using context: Context[T]): Unit = {
        context.subCommands ::= CommandUtil.createNextCommand(CommandUtil.createResultCommand(result),
            command => if (allowNonEmptyCommand) List.empty else command)
    }

    def argument[T, A](argument: Argument[A])(builder: A => Function[T])(using context: Context[T]): Unit =
        context.subCommands ::= new Command[T] {
            override def run(command: List[String]): Option[T] = {
                if (command.length < 1)
                    return None;

                CommandUtil.forArgument(argument, command) { value =>
                    return build(builder(value)).run(command.tail)
                }
            }

            override def getSuggestions(command: List[String]): List[String] = {
                CommandUtil.forArgument(argument, command) { value =>
                    return CommandUtil.getSuggestionsForMatchingArgument(command, argument, value, builder)
                }

                val argumentPart = if (command.isEmpty) "" else command(0)

                argument.getSuggestions(argumentPart)
            }
        }

    def optionalArgument[T, A](argument: Argument[A])(builder: Option[A] => Function[T])(using context: Context[T]): Unit =
        context.subCommands ::= new Command[T] {
            override def run(command: List[String]): Option[T] = {
                CommandUtil.forArgument(argument, command) { value =>
                    return build(builder(Some(value))).run(command.tail)
                }

                build(builder(None)).run(command.tail);
            }

            override def getSuggestions(command: List[String]): List[String] = {
                CommandUtil.forArgument(argument, command) { value =>
                    return CommandUtil.getSuggestionsForMatchingArgument(command, argument, value, a => builder(Some(a)))
                }

                val argumentPart = if (command.isEmpty) "" else command(0)
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
                build(builder(CommandUtil.hasFlag(command, argument)))
                    .run(CommandUtil.removeFirstMatchingArgument(command, argument))
            }

            override def getSuggestions(command: List[String]): List[String] = {
                val hasFlag = CommandUtil.hasFlag(command, argument)

                if (!hasFlag) {
                    val argumentPart = if (command.isEmpty) "" else command(0)

                    val suggestions = argument.getSuggestions(argumentPart)

                    if (!suggestions.isEmpty)
                        return suggestions
                }

                build(builder(hasFlag)).getSuggestions(CommandUtil.removeFirstMatchingArgument(command, argument))
            }
        }

    def withFlags[T, K](flags: Map[K, Argument[Unit]])(builder: Map[K, Boolean] => Function[T])(using context: Context[T]): Unit = {
        context.subCommands ::= new Command[T] {
            override def run(command: List[String]): Option[T] = {
                build(builder(CommandUtil.hasFlags(command, flags)))
                    .run(CommandUtil.removeFlags(command, flags))
            }

            override def getSuggestions(command: List[String]): List[String] = {
                val hasFlags = CommandUtil.hasFlags(command, flags)

                val argumentPart = if (command.isEmpty) "" else command(0)
                val suggestions = flags.flatMap(_._2.getSuggestions(argumentPart))

                if (!suggestions.isEmpty)
                    return suggestions.toList

                build(builder(hasFlags)).getSuggestions(CommandUtil.removeFlags(command, flags))
            }
        }
    }

    def literalFlag[T](flag: String, shortFlag: Option[Char] = None)(builder: Function[T])(using context: Context[T]): Unit = {
        val argument = CommandArgument.flag(flag, shortFlag)

        context.subCommands ::= new Command[T] {
            override def run(command: List[String]): Option[T] = {
                if (command.isEmpty)
                    return None;

                if (CommandUtil.hasFlag(command, argument))
                    return build(builder).run(CommandUtil.removeFirstMatchingArgument(command, argument))

                None
            }

            override def getSuggestions(command: List[String]): List[String] = {
                if (!CommandUtil.hasFlag(command, argument)) {
                    val argumentPart = if (command.isEmpty) "" else command(0)

                    return argument.getSuggestions(argumentPart)
                }

                build(builder).getSuggestions(CommandUtil.removeFirstMatchingArgument(command, argument))
            }
        }
    }

    def argumentFlag[T, A](flag: String, shortFlag: Option[Char] = None, argument: Argument[A])
                          (builder: A => Function[T])(using context: Context[T]): Unit = {
        val flagArgument = CommandArgument.flag(flag, shortFlag)

        context.subCommands ::= new Command[T] {
            override def run(command: List[String]): Option[T] = {
                if (command.length < 1)
                    return None;

                val resultArgument = CommandUtil.getArgumentFlagResult(flagArgument, argument.get(_), command)

                for ((result, i, used) <- resultArgument)
                    return build(builder(result)).run(command.patch(i, Nil, used))

                None
            }

            override def getSuggestions(command: List[String]): List[String] = {
                val commandPart = if (command.isEmpty) "" else command(0)
                val commandParts = commandPart.split("=").toList

                val flagSuggestions =
                    CommandUtil.getArgumentFlagSuggestions(flagArgument, argument.getSuggestions(_), commandParts, command)

                val resultArgument = CommandUtil.getArgumentFlagResult(flagArgument, argument.get(_), command)

                for ((result, i, used) <- resultArgument)
                    return build(builder(result)).getSuggestions(command.drop(used)) ::: flagSuggestions

                flagSuggestions
            }
        }
    }
}
