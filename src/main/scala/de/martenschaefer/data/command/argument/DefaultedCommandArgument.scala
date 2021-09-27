package de.martenschaefer.data.command.argument

class DefaultedCommandArgument[+T](argument: CommandArgument[T], alternative: => T) extends CommandArgument[T] {
    override val name: String = this.argument.name

    override def get(argument: String): Option[T] = this.argument.get(argument)
        .orElse(Some(this.alternative))

    override def getSuggestions(argument: String): List[String] = {
        val suggestions = this.argument.getSuggestions(argument)

        if (suggestions.isEmpty)
            List("")
        else
            suggestions
    }
}
