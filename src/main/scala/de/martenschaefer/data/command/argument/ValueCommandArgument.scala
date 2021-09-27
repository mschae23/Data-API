package de.martenschaefer.data.command.argument

class ValueCommandArgument[+T](override val name: String, val f: String => Option[T]) extends CommandArgument[T] {
    override def get(argument: String): Option[T] = this.f(argument)

    override def getSuggestions(argument: String): List[String] = List.empty
}
