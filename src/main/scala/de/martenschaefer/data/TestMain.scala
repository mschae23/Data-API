package de.martenschaefer.data

import de.martenschaefer.data.command.Command
import de.martenschaefer.data.command.argument.CommandArgument as Argument
import de.martenschaefer.data.command.builder.CommandBuilder.*
import de.martenschaefer.data.lang.LangLexer
import de.martenschaefer.data.registry.impl.SimpleRegistry
import de.martenschaefer.data.registry.{ Registry, RegistryKey }
import de.martenschaefer.data.serialization.Element.*
import de.martenschaefer.data.serialization.{ Codec, JsonCodecs }
import de.martenschaefer.data.util.{ DataResult, Identifier, Lifecycle }

object TestMain {
    def main(args: Array[String]): Unit = {
        val testString =
            """
              |{
              |    "test": 3,
              |    "test_2": {
              |        test_operators: 3 + 1,
              |        "test_function": feature(2, 4).apply({})
              |    }
              |}""".stripMargin

        val tokens = LangLexer.getTokens(testString)
        println(tokens)
    }
}
