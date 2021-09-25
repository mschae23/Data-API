package de.martenschaefer.data.command.argument

class LiteralArgument(val literal: String, val ignoreCase: Boolean = false) extends CommandArgument[Unit] {
    override val name: String = this.literal

    override def get(argument: String): Option[Unit] =
        if (this.matches(argument))
            Some(())
        else
            None

    private def matches(argument: String): Boolean =
        if (this.ignoreCase)
            this.literal.equalsIgnoreCase(argument)
        else
            this.literal == argument
}
