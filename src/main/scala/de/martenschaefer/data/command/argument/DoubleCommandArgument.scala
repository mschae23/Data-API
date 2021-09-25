package de.martenschaefer.data.command.argument

class DoubleCommandArgument(override val name: String) extends CommandArgument[Double] {
    override def get(argument: String): Option[Double] = argument.toDoubleOption
}
