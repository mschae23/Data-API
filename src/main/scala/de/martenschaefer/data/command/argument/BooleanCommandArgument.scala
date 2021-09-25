package de.martenschaefer.data.command.argument

class BooleanCommandArgument(override val name: String) extends CommandArgument[Boolean] {
    override def get(argument: String): Option[Boolean] = argument.toBooleanOption
}
