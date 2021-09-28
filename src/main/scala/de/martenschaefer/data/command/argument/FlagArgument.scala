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

    override def getSuggestions(argument: String): List[String] = {
        if (argument.startsWith("--")) {
            if (this.flag.startsWith(argument.substring(2)))
                List("--" + this.flag)
            else
                List.empty
        } else if (this.shortFlag.isDefined && argument.matches("-[a-z]+")
            && !argument.matches("-[a-z]*=.*")) {
            if (argument.contains(this.shortFlag.get))
                List(argument)
            else
                List(argument + this.shortFlag.get)
        } else if ("-" == argument) {
            if (this.shortFlag.isDefined)
                List("--" + this.flag, "-" + this.shortFlag.get)
            else
                List("--" + this.flag)
        } else
            List.empty
    }
}
