package de.martenschaefer.data.command.builder

import de.martenschaefer.data.command.Command

case class CommandBuilderContext[T](val command: List[String],
                                    var subCommands: List[(Command[T], List[String])])
