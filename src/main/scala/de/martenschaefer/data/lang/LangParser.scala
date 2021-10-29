package de.martenschaefer.data.lang

import scala.collection.immutable.ListMap
import scala.collection.mutable.ListBuffer
import de.martenschaefer.data.lang.LangParser.*
import de.martenschaefer.data.serialization.{ ElementError, ElementNode }

class LangParser private(private val input: ListBuffer[LangToken],
                         private var prefixParselets: ListMap[LangToken => Boolean, PrefixParselet],
                         private var parselets: ListMap[LangToken => Boolean, Parselet]) {
    def this(input: List[LangToken]) =
        this(ListBuffer.from(input), ListMap.empty, ListMap.empty)

    def next(): Option[LangToken] = this.input.headOption match {
        case Some(token) =>
            this.input.remove(0)
            Some(token)

        case None => None
    }

    def peek: Option[LangToken] = this.input.headOption

    def expect(token: LangToken): Either[List[ElementError], Unit] = {
        val option = this.peek

        if (option.contains(token)) {
            this.next()
            Right(())
        } else
            Left(List(UnexpectedTokenError(token, option.getOrElse(LangToken.EndOfFile))))
    }

    def matches(token: LangToken): Boolean =
        this.peek.contains(token)

    private def parsePrefix(token: LangToken): Result = {
        for ((predicate, parselet) <- this.prefixParselets) {
            if (predicate(token))
                return parselet.parse(this, token)
        }

        Left(List(UnresolvedTokenError(token)))
    }

    private def getParselet(token: LangToken): Option[Parselet] = {
        for ((predicate, parselet) <- this.parselets) {
            if (predicate(token)) return Some(parselet)
        }

        None
    }

    private def parse(left: LangExpression, token: LangToken): Result = {
        for ((predicate, parselet) <- this.parselets) {
            if (predicate(token))
                return parselet.parse(this, left, token)
        }

        Left(List(UnresolvedTokenError(token)))
    }

    private def getPrecedence: Int = {
        this.peek.flatMap(this.getParselet).map(_.precedence).getOrElse(0)
    }

    def parseExpression(precedence: Int): Result = {
        var token = this.next() match {
            case Some(token) => token
            case None => return Left(List(UnexpectedEofError()))
        }

        var left: LangExpression = this.parsePrefix(token) match {
            case Right(expr) => expr
            case result: Left[_, _] => return result
        }

        while (precedence < getPrecedence) {
            token = this.next() match {
                case Some(t) => t
                case None => return Right(left)
            }

            left = this.parse(left, token) match {
                case Right(expr) => expr
                case result: Left[_, _] => return result
            }
        }

        Right(left)
    }

    //noinspection AccessorLikeMethodIsEmptyParen
    def getExpression(): Result = {
        this.parseExpression(-1)
    }

    private def matchesFunction(token: LangToken, name: String): Boolean = {
        token match {
            case LangToken.FunctionName(functionName) => name == functionName
            case _ => false
        }
    }

    def registerPrefix(tokenPredicate: LangToken => Boolean, parselet: PrefixParselet): Unit = {
        this.prefixParselets = this.prefixParselets.updated(tokenPredicate, parselet)
    }

    def registerPrefix(name: String, parselet: PrefixParselet): Unit =
        this.registerPrefix(matchesFunction(_, name), parselet)

    def register(tokenPredicate: LangToken => Boolean, parselet: Parselet): Unit = {
        this.parselets = this.parselets.updated(tokenPredicate, parselet)
    }

    def register(name: String, parselet: Parselet): Unit =
        this.register(matchesFunction(_, name), parselet)

    def registerBuiltinParselets(): Unit = {
        this.registerPrefix(_ == LangToken.ArrayStart, ArrayParselet())
        this.registerPrefix(_ == LangToken.ObjectStart, ObjectParselet())
        this.registerPrefix(_ == LangToken.ParenthesesOpen, GroupParselet())

        this.register(":", TupleParselet())
        this.register(_ == LangToken.ParenthesesOpen, FunctionCallParselet())
        this.register(".", ObjectSyntaxFunctionCallParselet())
    }

    def registerDefaultPrimitiveParselet(): Unit = {
        this.registerPrefix(_ => true, PrimitiveParselet)
    }
}

object LangParser {
    type Result = Either[List[ElementError], LangExpression]

    case class UnresolvedTokenError(token: LangToken, override val path: List[ElementNode] = List.empty) extends ElementError(path) {
        override def getDescription(path: String): String =
            s"Unresolved token ${this.token}"

        override def mapPath(f: List[ElementNode] => List[ElementNode]): ElementError =
            UnresolvedTokenError(this.token, f(this.path))
    }

    case class UnexpectedTokenError(expected: LangToken, got: LangToken, override val path: List[ElementNode] = List.empty) extends ElementError(path) {
        override def getDescription(path: String): String =
            s"Unexpected token: expected ${this.expected}, got ${this.got}"

        override def mapPath(f: List[ElementNode] => List[ElementNode]): ElementError =
            UnexpectedTokenError(this.expected, this.got, f(this.path))
    }

    case class UnexpectedEofError(override val path: List[ElementNode] = List.empty) extends ElementError(path) {
        override def getDescription(path: String): String =
            "Unexpected EOF"

        override def mapPath(f: List[ElementNode] => List[ElementNode]): ElementError =
            UnexpectedEofError(f(this.path))
    }
}
