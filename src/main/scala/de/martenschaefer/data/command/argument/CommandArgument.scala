package de.martenschaefer.data.command.argument

import de.martenschaefer.data.util.Identifier

trait CommandArgument[+T] {
    val name: String

    def get(argument: String): Option[T]
}

object CommandArgument {
    def string(name: String): CommandArgument[String] = new StringCommandArgument(name)

    def int(name: String): CommandArgument[Int] = new IntCommandArgument(name)
    def long(name: String): CommandArgument[Long] = new LongCommandArgument(name)

    def float(name: String): CommandArgument[Float] = new FloatCommandArgument(name)
    def double(name: String): CommandArgument[Double] = new DoubleCommandArgument(name)

    def boolean(name: String): CommandArgument[Boolean] = new BooleanCommandArgument(name)

    def identifier(name: String, defaultNamespace: Option[String] = None): CommandArgument[Identifier] =
        new IdentifierCommandArgument(name, defaultNamespace)

    def literal(literal: String, ignoreCase: Boolean = false): CommandArgument[Unit] =
        new LiteralArgument(literal, ignoreCase)

    def flag(flag: String, shortFlag: Option[Char] = None): CommandArgument[Unit] =
        new FlagArgument(flag, shortFlag)
}
