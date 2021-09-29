package de.martenschaefer.data.command.builder

import de.martenschaefer.data.Result
import de.martenschaefer.data.command.Command
import de.martenschaefer.data.command.argument.CommandArgument
import de.martenschaefer.data.command.util.{ CommandError, CommandUtil }
import de.martenschaefer.data.serialization.ElementError
import de.martenschaefer.data.util.DataResult.*
import de.martenschaefer.data.util.Lifecycle

object CommandBuilder {
    type Context[T] = CommandBuilderContext[T]
    type Argument[T] = CommandArgument[T]
    type Function[T] = Context[T] ?=> Unit

    def build[T](builder: Function[T]): Command[T] = {
        val context = new Context[T](List.empty)
        builder(using context)
        val subCommands = context.subCommands.reverse

        new Command[T] {
            override def run(command: List[String]): Result[T] = {
                var errors: List[ElementError] = List.empty
                var lifecycle = Lifecycle.Stable

                for (i <- 0 until subCommands.length) {
                    val subCommand = subCommands(i)

                    subCommand.run(command) match {
                        case Success(commandResult, l) => return Success(commandResult, l)
                        case Failure(e, l) => {
                            errors = errors ::: e
                            lifecycle += l
                        }
                    }
                }

                Failure(errors.sortWith((error1, error2) => {
                    if (!error1.isInstanceOf[CommandError] || !error2.isInstanceOf[CommandError])
                        true
                    else
                        error1.asInstanceOf[CommandError].command.length < error2.asInstanceOf[CommandError].command.length
                }), lifecycle)
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
            override def run(command: List[String]): Result[T] = {
                if (command.length < 1)
                    return Failure(List(CommandError.ArgumentNotMatchedError(command, argument.name)));

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
            override def run(command: List[String]): Result[T] = {
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

    def withFlag[T](flag: String, shortFlag: Option[Char] = None)(builder: Boolean => Function[T])(using context: Context[T]): Unit = {
        val argument = CommandArgument.flag(flag, shortFlag)

        context.subCommands ::= new Command[T] {
            override def run(command: List[String]): Result[T] = {
                val (hasFlag, i, used) = CommandUtil.hasArgument(command, argument)
                val flagPart = if (command.length <= i) "" else command(i)
                val patchedArgument = CommandUtil.getPatchedFlag(shortFlag, flagPart)

                build(builder(hasFlag)).run(command.patch(i, patchedArgument, used))
            }

            override def getSuggestions(command: List[String]): List[String] = {
                val (hasFlag, i, used) = CommandUtil.hasArgument(command, argument)

                val flagPart = if (command.length <= i) "" else command(i)
                val patchedArgument = CommandUtil.getPatchedFlag(shortFlag, flagPart)

                val suggestions = if (!hasFlag || command.drop(i + used).isEmpty) {
                    val argumentPart = if (command.isEmpty) "" else command(command.length - 1)
                    val suggestions = argument.getSuggestions(argumentPart)

                    if (hasFlag && patchedArgument.isEmpty)
                        return suggestions
                    else
                        suggestions
                } else List.empty

                build(builder(hasFlag)).getSuggestions(command.patch(i, patchedArgument, used)) ::: suggestions
            }
        }
    }

    def withFlags[T, K](flags: Map[K, (String, Option[Char])])(builder: Map[K, Boolean] => Function[T])(using context: Context[T]): Unit = {
        val flagArguments = flags.view.mapValues(flag => CommandArgument.flag(flag._1, flag._2)).toMap
        val flags2 = flags.map[(Argument[Unit], Option[Char])]((k, flag) => (flagArguments(k), flag._2)).toList

        context.subCommands ::= new Command[T] {
            override def run(command: List[String]): Result[T] = {
                build(builder(CommandUtil.hasFlags(command, flagArguments)))
                    .run(CommandUtil.removeFlags(command, flags2))
            }

            override def getSuggestions(command: List[String]): List[String] = {
                val hasFlags = CommandUtil.hasFlags(command, flagArguments)

                val argumentPart = if (command.isEmpty) "" else command(command.length - 1)
                val suggestions = flagArguments.flatMap(_._2.getSuggestions(argumentPart))

                build(builder(hasFlags)).getSuggestions(CommandUtil.removeFlags(command, flags2)) ::: suggestions.toList
            }
        }
    }

    def literalFlag[T](flag: String, shortFlag: Option[Char] = None)(builder: Function[T])(using context: Context[T]): Unit = {
        val argument = CommandArgument.flag(flag, shortFlag)

        context.subCommands ::= new Command[T] {
            override def run(command: List[String]): Result[T] = {
                if (command.isEmpty)
                    return Failure(List(CommandError.FlagNotFoundError(command, flag)));

                val (hasFlag, i, used) = CommandUtil.hasArgument(command, argument)

                if (hasFlag)
                    return build(builder).run(command.patch(i, CommandUtil.getPatchedFlag(shortFlag, command(i)), used))

                Failure(List(CommandError.FlagNotFoundError(command, flag)))
            }

            override def getSuggestions(command: List[String]): List[String] = {
                val (hasFlag, i, used) = CommandUtil.hasArgument(command, argument)

                val suggestions = if (!hasFlag || command.drop(i + used).isEmpty) {
                    val argumentPart = if (command.isEmpty) "" else command(i)

                    argument.getSuggestions(argumentPart)
                } else List.empty

                suggestions ::: build(builder).getSuggestions(command.patch(i,
                    CommandUtil.getPatchedFlag(shortFlag, command(i)), used))
            }
        }
    }

    def argumentFlag[T, A](flag: String, shortFlag: Option[Char] = None, argument: Argument[A])
                          (builder: A => Function[T])(using context: Context[T]): Unit = {
        val flagArgument = CommandArgument.flag(flag, shortFlag)

        context.subCommands ::= new Command[T] {
            override def run(command: List[String]): Result[T] = {
                if (command.length < 1)
                    return Failure(List(CommandError.FlagArgumentNotFoundError(command, flag, argument.name)));

                val resultArgument = CommandUtil.getArgumentFlagResult(flagArgument, argument.get(_), command)

                for ((result, i, used) <- resultArgument) {
                    val patchedCommand = if (used == 2) {
                        val patchedArgument = CommandUtil.getPatchedFlag(shortFlag, command(i))
                        command.patch(i, patchedArgument, used)
                    } else command.patch(i, Nil, used)

                    return build(builder(result)).run(patchedCommand)
                }

                Failure(List(CommandError.FlagArgumentNotFoundError(command, flag, argument.name)))
            }

            override def getSuggestions(command: List[String]): List[String] = {
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

                    return flagSuggestions ::: build(builder(result)).getSuggestions(patchedCommand)
                }

                flagSuggestions
            }
        }
    }
}
