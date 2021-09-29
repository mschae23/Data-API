package de.martenschaefer.data.command

import de.martenschaefer.data.Result
import de.martenschaefer.data.command.builder.{ CommandBuilder, CommandBuilderContext }
import de.martenschaefer.data.serialization.ElementError
import de.martenschaefer.data.util.DataResult

trait Command[+T] {
    /**
     * Runs this command.
     * @param command The arguments for this command.
     * @return {@code Some(result)} if successful, otherwise {@code None}
     */
    def run(command: List[String]): Result[T]

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
