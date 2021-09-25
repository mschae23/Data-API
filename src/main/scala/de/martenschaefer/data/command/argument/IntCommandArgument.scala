package de.martenschaefer.data.command.argument

class IntCommandArgument(override val name: String) extends CommandArgument[Int] {
    override def get(argument: String): Option[Int] = argument.toIntOption
}
