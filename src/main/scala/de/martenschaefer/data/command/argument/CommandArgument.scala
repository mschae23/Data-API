package de.martenschaefer.data.command.argument

import de.martenschaefer.data.registry.Registry
import de.martenschaefer.data.util.Identifier

trait CommandArgument[+T] {
    val name: String

    /**
     * Tries to get a value of type {@link T} from the given {@code String}.
     *
     * @param argument the command argument
     * @return {@code Some(value)} if successful, otherwise {@code None}
     */
    def get(argument: String): Option[T]

    /**
     * Returns a list of suggestions for how the argument could be completed.
     *
     * @param argument the incomplete command argument
     * @return the list of suggestions
     */
    def getSuggestions(argument: String): List[String]
}

object CommandArgument {
    def string(name: String): CommandArgument[String] = new ValueCommandArgument(name, Some(_))

    def int(name: String): CommandArgument[Int] = new ValueCommandArgument(name, _.toIntOption)
    def long(name: String): CommandArgument[Long] = new ValueCommandArgument(name, _.toLongOption)

    def float(name: String): CommandArgument[Float] = new ValueCommandArgument(name, _.toFloatOption)
    def double(name: String): CommandArgument[Double] = new ValueCommandArgument(name, _.toDoubleOption)

    def boolean(name: String): CommandArgument[Boolean] = new ValueCommandArgument(name, _.toBooleanOption)

    def identifier(name: String, suggestions: Identifier => List[Identifier],
                   defaultNamespace: Option[String] = None): CommandArgument[Identifier] =
        new IdentifierCommandArgument(name, suggestions, defaultNamespace)

    def identifier(name: String, defaultNamespace: Option[String]): CommandArgument[Identifier] =
        identifier(name, _ => List.empty, defaultNamespace)

    def identifier(name: String): CommandArgument[Identifier] =
        identifier(name, None)

    def literal(literal: String, ignoreCase: Boolean = false): CommandArgument[Unit] =
        new LiteralArgument(literal, ignoreCase)

    def flag(flag: String, shortFlag: Option[Char] = None): CommandArgument[Unit] =
        new FlagArgument(flag, shortFlag)

    def map[K, V](name: String, argumentK: CommandArgument[K], map: K => Option[V]): CommandArgument[V] =
        new MapCommandArgument(name, argumentK, map)

    def fromMap[K, V](name: String, argumentK: CommandArgument[K], mapKV: Map[K, V]): CommandArgument[V] =
        map(name, argumentK, mapKV.get(_))

    def fromRegistry[T](name: String, defaultNamespace: Option[String] = None, registry: Registry[T]): CommandArgument[T] =
        map(name, identifier(s"$name ID", registry.getSuggestions(_), defaultNamespace), registry.get(_))

    def withDefault[T](argument: CommandArgument[T], alternative: => T): CommandArgument[T] =
        new DefaultedCommandArgument(argument, alternative)
}
