package de.martenschaefer.data.command

import de.martenschaefer.data.command.builder.{ CommandBuilder, CommandBuilderContext }

trait Command[+T] {
    /**
     * Runs this command.
     * @param command The arguments for this command.
     * @return {@code Some(result)} if successful, otherwise {@code None}
     */
    def run(command: List[String]): Option[T]

    /**
     * Returns a list of suggestions for the next command part.
     * @param command The incomplete arguments for this command.
     * @return the list of suggestions
     */
    def getSuggestions(command: List[String]): List[String]
}

object Command {
    def build[T](builder: CommandBuilderContext[T] ?=> Unit): Command[T] = CommandBuilder.build(builder)
}
