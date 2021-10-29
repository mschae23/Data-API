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

    private val functionStopChars = List('.', ':', ',', '=',
        '(', ')', '{', '}', '[', ']',
        '+', '-', '*', '/', '%',
        '&', '|', '!', '?', '\\', '<', '>', '$', '#', '~')

    private def parseFunctionName(input: LexerString, tokens: ListBuffer[LangToken], firstC: Char): Unit = {
        val functionNameBuilder = new StringBuilder()
        functionNameBuilder.append(firstC)

        var continue = !functionStopChars.contains(firstC)
        var c = firstC

        while (continue) {
            c = input.peek match {
                case Some(c) => c
                case _ => continue = false; ' '
            }

            val isFunctionStopChar = functionStopChars.contains(c)

            if (!c.isWhitespace && !isFunctionStopChar) {
                functionNameBuilder.append(c)
                input.next()
            } else {
                continue = false
            }
        }

        tokens.append(LangToken.FunctionStart(functionNameBuilder.toString()))
    }

    def parseEscapedString(input: LexerString): Either[List[ElementError], String] = {
        val c = input.next() match {
            case Some(c) => c
            case None => return Left(List(ValidationError(_ => s"EOF while parsing string")))
        }

        c match {
            case '"' => Right("\"")
            case '\\' => Right("\\")
            case '/' => Right("/")
            case 'b' => Right("\b")
            case 'f' => Right("\f")
            case 'n' => Right("\n")
            case 'r' => Right("\r")
            case 't' => Right("\t")

            case 'u' =>
                val string = new StringBuilder()

                for (i <- 0 until 4) {
                    val c = input.next()

                    if (c.isEmpty)
                        return Left(List(ValidationError(_ => s"EOF while parsing string, Unicode escape sequence")))
                    else if (!c.get.toString.matches("[0-9a-fA-F]"))
                        return Left(List(ValidationError(_ => s"Unexpected character in Unicode escape sequence: ${c.get}")))

                    string.append(c.get)
                }

                val number = try {
                    Integer.parseInt(string.toString(), 16)
                } catch {
                    case e: NumberFormatException => return Left(List(ValidationError(_ => s"Invalid Unicode escape sequence: ${string.toString()}")))
                }

                Right(Character.toString(number))

            case _ => Left(List(ValidationError(_ => s"Unexpected escape character '$c'")))
        }
    }

    def parseString(input: LexerString): Either[List[ElementError], String] = {
        val string = new StringBuilder()

        var c = input.next().getOrElse(return Left(List(ValidationError(_ => "EOF while parsing string", List.empty))))

        while (c != '"') {
            if (c == '\n')
                return Left(List(ValidationError(_ => "New line while parsing string", List.empty)))

            if (c == '\\') {
                parseEscapedString(input) match {
                    case Right(s) => string.append(s)
                    case Left(errors) => return Left(errors)
                }
            } else
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
                            if (structures.isEmpty) "Tried to close object without '{'"
                            else s"Tried to close object, but was in ${structures.head}", List.empty)))

                    structures.remove(0)
                    tokens.append(LangToken.ObjectEnd)

                case '[' =>
                    structures.prepend(StructureType.Array)
                    tokens.append(LangToken.ArrayStart)
                case ']' =>
                    if (!structures.headOption.contains(StructureType.Array))
                        return Left(List(ValidationError(_ =>
                            if (structures.isEmpty) "Tried to close array without '['"
                            else s"Tried to close array, but was in ${structures.head}", List.empty)))

                    structures.remove(0)
                    tokens.append(LangToken.ArrayEnd)

                case '(' => structures.prepend(StructureType.Parentheses)
                    tokens.append(LangToken.ParenthesesOpen)
                case ')' =>
                    if (!structures.headOption.contains(StructureType.Parentheses))
                        return Left(List(ValidationError(_ =>
                            if (structures.isEmpty) "Tried to close parentheses without '('"
                            else s"Tried to close parentheses, but was in ${structures.head}", List.empty)))

                    structures.remove(0)
                    tokens.append(LangToken.ParenthesesClose)

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
