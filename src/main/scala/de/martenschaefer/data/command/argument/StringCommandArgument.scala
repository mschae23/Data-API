package de.martenschaefer.data.command.argument

class StringCommandArgument(override val name: String) extends CommandArgument[String] {
    override def get(argument: String): Option[String] = Some(argument)
}
