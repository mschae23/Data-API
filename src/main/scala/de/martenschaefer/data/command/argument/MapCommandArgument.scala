package de.martenschaefer.data.command.argument

class MapCommandArgument[K, +V](override val name: String, val argumentK: CommandArgument[K], val map: K => Option[V]) extends CommandArgument[V] {
    override def get(argument: String): Option[V] = this.argumentK.get(argument)
        .flatMap(this.map(_))

    export this.argumentK.getSuggestions
}
