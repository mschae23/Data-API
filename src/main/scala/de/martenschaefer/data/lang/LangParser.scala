package de.martenschaefer.data.lang

import scala.collection.immutable.ListMap
import scala.collection.mutable.ListBuffer
import de.martenschaefer.data.lang.LangParser.*
import de.martenschaefer.data.serialization.{ ElementError, ValidationError }

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
        val option = this.next()

        if (option.contains(token)) Right(())
        else Left(List(ValidationError(
            _ => s"Expected '$token', got: ${option.getOrElse("EOF")}'", List.empty)))
    }

    def matches(token: LangToken): Boolean =
        this.peek.contains(token)

    private def parsePrefix(token: LangToken): Option[LangExpression] = {
        for ((predicate, parselet) <- this.prefixParselets) {
            if (predicate(token))
                return parselet.parse(this, token)
        }

        None
    }

    private def getParselet(token: LangToken): Option[Parselet] = {
        for ((predicate, parselet) <- this.parselets) {
            if (predicate(token)) return Some(parselet)
        }

        None
    }

    private def parse(left: LangExpression, token: LangToken): Option[LangExpression] = {
        for ((predicate, parselet) <- this.parselets) {
            if (predicate(token))
                return parselet.parse(this, left, token)
        }

        None
    }

    private def getPrecedence: Int = {
        this.peek.flatMap(this.getParselet).map(_.precedence).getOrElse(0)
    }

    def parseExpression(precedence: Int): Result = {
        var token = this.next() match {
            case Some(token) => token
            case None => return Left(List(ValidationError(_ => "Unexpected EOF")))
        }

        var left: LangExpression = this.parsePrefix(token) match {
            case Some(expr) => expr
            case None => return Left(List(ValidationError(_ => s"Token $token could not get resolved")))
        }

        while (precedence < getPrecedence) {
            token = this.next() match {
                case Some(t) => t
                case None => println(s"Done: $left"); return Right(left)
            }

            left = this.parse(left, token) match {
                case Some(expr) => expr
                case None => return Left(List(ValidationError(_ => s"Token $token could not get resolved")))
            }
        }

        Right(left)
    }

    def getExpression(): Result = {
        this.parseExpression(-1)
    }

    private def matchesFunction(token: LangToken, name: String): Boolean = {
        token match {
            case LangToken.FunctionStart(functionName) => name == functionName
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
}

object LangParser {
    type Result = Either[List[ElementError], LangExpression]
}
