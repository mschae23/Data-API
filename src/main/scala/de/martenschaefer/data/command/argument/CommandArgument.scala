package de.martenschaefer.data.command.argument

import de.martenschaefer.data.registry.Registry
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

    def map[K, V](name: String, argumentK: CommandArgument[K], map: K => Option[V]): CommandArgument[V] =
        new MapCommandArgument(name, argumentK, map)

    def fromMap[K, V](name: String, argumentK: CommandArgument[K], mapKV: Map[K, V]): CommandArgument[V] =
        map(name, argumentK, mapKV.get(_))

    def fromRegistry[T](name: String, defaultNamespace: Option[String] = None, registry: Registry[T]): CommandArgument[T] =
        map(name, identifier(s"$name ID", defaultNamespace), registry.get(_))

    def withDefault[T](argument: CommandArgument[T], alternative: => T): CommandArgument[T] =
        new DefaultedCommandArgument(argument, alternative)
}
