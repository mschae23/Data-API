package de.martenschaefer.data.command.argument

import de.martenschaefer.data.util.Identifier

class IdentifierCommandArgument(override val name: String, val defaultNamespace: Option[String] = None) extends CommandArgument[Identifier] {
    override def get(argument: String): Option[Identifier] =
        Some(Identifier.withDefaultNamespace(argument, this.defaultNamespace.getOrElse(return None)))
}
