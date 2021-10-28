package de.martenschaefer.data.lang

import scala.collection.mutable.ListBuffer
import de.martenschaefer.data.lang.impl.LexerString
import de.martenschaefer.data.serialization.{ ElementError, ValidationError }

object LangLexer {
    private type Result = Either[List[ElementError], List[LangToken]]

    extension (self: LexerString) {
        def expect(c: Char): Either[List[ElementError], LexerString] = {
            val option = self.peek

            if (option.contains(c)) {
                self.next()
                Right(self)
            } else Left(List(ValidationError(
                _ => s"Expected '$c', got: ${option.map(c => s"'$c''").getOrElse("EOF")}'", List.empty)))
        }

        def expect(s: String): Either[List[ElementError], String] = {
            for (c <- s) {
                self.expect(c) match {
                    case Left(errors) => return Left(errors)
                    case _ =>
                }
            }

            Right(s)
        }

        def skipWhitespace(): Unit = {
            while (true) {
                val c = self.peek.getOrElse(return)

                if (!c.isWhitespace) {
                    return
                }

                self.next()
            }
        }
    }

    private val functionStopChars = List('.', ':', ',', '=', '(', ')', '{', '}', '[', ']')

    private def parseFunctionName(input: LexerString, tokens: ListBuffer[LangToken], firstC: Char): Unit = {
        if (functionStopChars.contains(firstC)) {
            tokens.append(LangToken.FunctionStart(firstC.toString))
            return
        }

        val functionNameBuilder = new StringBuilder()
        functionNameBuilder.append(firstC)
        var continue = true

        while (continue) {
            val c = input.peek.getOrElse {
                tokens.append(LangToken.FunctionStart(functionNameBuilder.toString()))
                return
            }

            if (!c.isWhitespace && !functionStopChars.contains(c)) {
                functionNameBuilder.append(c)
                input.next()
            } else {
                continue = false
            }
        }

        tokens.append(LangToken.FunctionStart(functionNameBuilder.toString()))
    }

    def parseString(input: LexerString): Either[List[ElementError], String] = {
        val string = new StringBuilder()

        var c = input.next().getOrElse(return Left(List(ValidationError(_ => "EOF while parsing string", List.empty))))

        while (c != '"') {
            if (c == '\n')
                return Left(List(ValidationError(_ => "New line while parsing string", List.empty)))

            string.append(c)

            c = input.next().getOrElse(return Left(List(ValidationError(_ => "EOF while parsing string", List.empty))))
        }

        Right(string.toString())
    }

    def getExpression(input: LexerString): Result = {
        input.skipWhitespace()
        val tokens = new ListBuffer[LangToken]()
        val structures = new ListBuffer[StructureType]()

        while (true) {
            val c = input.next().getOrElse(return Right(tokens.toList))

            c match {
                case '{' =>
                    structures.prepend(StructureType.Object)
                    tokens.append(LangToken.ObjectStart)
                case '}' =>
                    if (!structures.headOption.contains(StructureType.Object))
                        return Left(List(ValidationError(_ =>
                            if (structures.isEmpty) "Tried to end object without '{'"
                            else s"Tried to end object, but was in ${structures.head}", List.empty)))

                    structures.remove(0)
                    tokens.append(LangToken.ObjectEnd)

                case '[' =>
                    structures.prepend(StructureType.Array)
                    tokens.append(LangToken.ArrayStart)
                case ']' =>
                    if (!structures.headOption.contains(StructureType.Array))
                        return Left(List(ValidationError(_ =>
                            if (structures.isEmpty) "Tried to end array without '['"
                            else s"Tried to end array, but was in ${structures.head}", List.empty)))

                    structures.remove(0)
                    tokens.append(LangToken.ArrayEnd)

                case '(' => structures.prepend(StructureType.FunctionArgs)
                    tokens.append(LangToken.FunctionArgsStart)
                case ')' =>
                    if (!structures.headOption.contains(StructureType.FunctionArgs))
                        return Left(List(ValidationError(_ =>
                            if (structures.isEmpty) "Tried to end function args without '('"
                            else s"Tried to end function args, but was in ${structures.head}", List.empty)))

                    structures.remove(0)
                    tokens.append(LangToken.FunctionArgsEnd)

                case ',' => tokens.append(LangToken.FieldEnd)

                case '"' => parseString(input) match {
                    case Right(string) => tokens.append(LangToken.StringToken(string))
                    case Left(errors) => return Left(errors)
                }

                case _ => parseFunctionName(input, tokens, c)
            }

            input.skipWhitespace()
        }

        Right(tokens.toList)
    }

    def getTokens(input: String): Result = {
        getExpression(new LexerString(input))
    }
}
