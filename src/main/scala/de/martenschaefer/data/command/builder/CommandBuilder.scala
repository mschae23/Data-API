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

                Failure(List(CommandError.NoMatchingSubcommandsError(command, errors.sortWith((error1, error2) => {
                    error1 match {
                        case e1: CommandError => error2 match {
                            case e2: CommandError =>
                                e1.getNoMatchingSubcommandsDepth > e2.getNoMatchingSubcommandsDepth
                            case _ => true
                        }

                        case _ => true
                    }
                }))), lifecycle)
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
                CommandUtil.getArgumentSuggestions(command, argument, builder)
            }
        }

    def optionalArgument[T, A](argument: Argument[A])(builder: Option[A] => Function[T])(using context: Context[T]): Unit =
        context.subCommands ::= new Command[T] {
            override def run(command: List[String]): Result[T] = {
                CommandUtil.forArgument(argument, command) { value =>
                    return build(builder(Some(value))).run(command.tail)
                }

                build(builder(None)).run(command);
            }

            override def getSuggestions(command: List[String]): List[String] = {
                val suggestions = CommandUtil.getArgumentSuggestions(command, argument, a => builder(Some(a)))

                if (suggestions.isEmpty) List("") else suggestions
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
                val flagPart = CommandUtil.getArgumentPart(command, i)
                val patchedArgument = CommandUtil.getPatchedFlag(shortFlag, flagPart)

                build(builder(hasFlag)).run(command.patch(i, patchedArgument, used))
            }

            override def getSuggestions(command: List[String]): List[String] = {
                val (hasFlag, i, used) = CommandUtil.hasArgument(command, argument)

                val flagPart = CommandUtil.getArgumentPart(command, i)
                val patchedArgument = CommandUtil.getPatchedFlag(shortFlag, flagPart)

                val suggestions = CommandUtil.forFlagSuggestion(command, hasFlag, i, used, true, argument) { suggestions =>
                    if (hasFlag && patchedArgument.isEmpty)
                        return suggestions
                    else
                        suggestions
                }

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

                val argumentPart = CommandUtil.getArgumentPart(command, command.length - 1)
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

                CommandUtil.forFlagSuggestion(command, hasFlag, i, used, false, argument)(s => s)
                    ::: build(builder).getSuggestions(command.patch(i,
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

                CommandUtil.getArgumentFlag(command, flag, shortFlag, flagArgument, argument, builder)
            }

            override def getSuggestions(command: List[String]): List[String] = {
                CommandUtil.getArgumentFlagSuggestions(command, flagArgument, shortFlag, argument, (resultOption, command) => {
                    resultOption match {
                        case Some(result) => build(builder(result)).getSuggestions(command)

                        case _ => List.empty
                    }
                })
            }
        }
    }

    def optionalArgumentFlag[T, A](flag: String, shortFlag: Option[Char] = None, argument: Argument[A])
                                  (builder: Option[A] => Function[T])(using context: Context[T]): Unit = {
        val flagArgument = CommandArgument.flag(flag, shortFlag)

        context.subCommands ::= new Command[T] {
            override def run(command: List[String]): Result[T] = {
                CommandUtil.getArgumentFlag(command, flag, shortFlag, flagArgument, argument, a => builder(Some(a)))
                    .flatOrElse(build(builder(None)).run(command))
            }

            override def getSuggestions(command: List[String]): List[String] = {
                CommandUtil.getArgumentFlagSuggestions(command, flagArgument, shortFlag, argument, (result, command) => {
                    build(builder(result)).getSuggestions(command)
                })
            }
        }
    }

    def defaultedArgumentFlag[T, A](flag: String, shortFlag: Option[Char] = None, argument: Argument[A], alternative: => A)
                                   (builder: A => Function[T])(using context: Context[T]): Unit = {
        optionalArgumentFlag(flag, shortFlag, argument) { result =>
            builder(result.getOrElse(alternative))
        }
    }
}
