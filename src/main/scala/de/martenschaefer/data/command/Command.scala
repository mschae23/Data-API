package de.martenschaefer.data.command

import de.martenschaefer.data.command.builder.{ CommandBuilder, CommandBuilderContext }

trait Command[+T] {
    def run(command: List[String]): Option[T]
}

object Command {
    def build[T](builder: CommandBuilderContext[T] ?=> Unit): Command[T] = CommandBuilder.build(builder)
}
