package de.martenschaefer.data.command.argument

class FloatCommandArgument(override val name: String) extends CommandArgument[Float] {
    override def get(argument: String): Option[Float] = argument.toFloatOption
}
