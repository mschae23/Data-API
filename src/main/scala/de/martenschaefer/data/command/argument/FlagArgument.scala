package de.martenschaefer.data.command.argument

class FlagArgument(val flag: String, val shortFlag: Option[Char] = None) extends CommandArgument[Unit] {
    override val name: String = this.flag

    override def get(argument: String): Option[Unit] =
        if (this.matches(argument))
            Some(())
        else
            None

    private def matches(argument: String): Boolean =
        "--" + this.flag == argument || (this.shortFlag.isDefined
            && argument.matches("^-[a-z]+") && argument.contains(this.shortFlag.get))
}
