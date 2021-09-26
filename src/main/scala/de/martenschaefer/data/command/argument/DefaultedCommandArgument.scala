package de.martenschaefer.data.command.argument

class DefaultedCommandArgument[+T](argument: CommandArgument[T], alternative: => T) extends CommandArgument[T] {
    override val name: String = this.argument.name

    override def get(argument: String): Option[T] = this.argument.get(argument)
        .orElse(Some(this.alternative))
}
