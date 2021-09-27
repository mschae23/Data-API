package de.martenschaefer.data.command.builder

import de.martenschaefer.data.command.Command

case class CommandBuilderContext[T](var subCommands: List[Command[T]])
