package de.martenschaefer.data.command.argument

class LongCommandArgument(override val name: String) extends CommandArgument[Long] {
    override def get(argument: String): Option[Long] = argument.toLongOption
}
