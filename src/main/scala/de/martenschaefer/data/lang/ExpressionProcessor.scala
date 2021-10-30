package de.martenschaefer.data.lang

import scala.collection.immutable.ListMap
import scala.collection.mutable.Stack
import cats.catsInstancesForId
import cats.data.Writer
import de.martenschaefer.data.lang.ExpressionProcessor.*
import de.martenschaefer.data.serialization.ElementError
import de.martenschaefer.data.util.*

class ExpressionProcessor private(private val expression: LangExpression,
                                  private val stack: Stack[LangExpression], // mutable
                                  private var processors: List[LangExpression => Result],
                                  private var postProcessors: List[LangExpression => Result]) {
    def this(expression: LangExpression) =
        this(expression, new Stack(), List.empty, List.empty)

    private def applyProcessors(expression: LangExpression, processors: List[LangExpression => Result]): Result = {
        var result: Result = Writer(List.empty, expression)

        for (processor <- processors)
            result = result.flatMap(processor)

        result
    }

    def processRecursive(): Result = {
        val expression = this.stack.headOption.getOrElse(return Writer(List.empty, this.expression))

        val resultExpression: Result = expression match {
            case LangExpression.ArrayLiteral(values) =>
                var result = this.applyProcessors(expression, this.processors)

                if (result.value != expression) {
                    this.stack.push(result.value)
                    this.processRecursive()
                } else {
                    var resultValues = List.empty[LangExpression]

                    for (value <- values.reverse) {
                        this.stack.push(value)
                        val valueResult = this.processRecursive()
                        result = result.flatMap(_ => valueResult)
                        resultValues ::= valueResult.value
                    }

                    val resultArray = LangExpression.ArrayLiteral(resultValues)
                    result.flatMap(_ => this.applyProcessors(resultArray, this.postProcessors))
                }

            case LangExpression.ObjectLiteral(fields) =>
                var result = this.applyProcessors(expression, this.processors)

                if (result.value != expression) {
                    this.stack.push(result.value)
                    this.processRecursive()
                } else {
                    var resultFields = ListMap.empty[LangExpression, LangExpression]

                    for ((key, value) <- fields.toList.reverse) {
                        this.stack.push(key)
                        val keyResult = this.processRecursive()
                        result = result.flatMap(_ => keyResult)
                        this.stack.push(value)
                        val valueResult = this.processRecursive()
                        result = result.flatMap(_ => valueResult)

                        resultFields = resultFields.updated(keyResult.value, valueResult.value)
                    }

                    val resultObject = LangExpression.ObjectLiteral(resultFields)
                    result.flatMap(_ => this.applyProcessors(resultObject, this.postProcessors))
                }

            case LangExpression.FunctionCall(function, args) =>
                var result = this.applyProcessors(expression, this.processors)

                if (result.value != expression) {
                    this.stack.push(result.value)
                    this.processRecursive()
                } else {
                    this.stack.push(function)
                    val functionResult = this.processRecursive()
                    result = result.flatMap(_ => functionResult)

                    var resultArgs = List.empty[LangExpression]

                    for (arg <- args.reverse) {
                        this.stack.push(arg)
                        val argResult = this.processRecursive()
                        result = result.flatMap(_ => argResult)
                        resultArgs ::= argResult.value
                    }

                    val resultFunction = LangExpression.FunctionCall(functionResult.value, args)
                    result.flatMap(_ => this.applyProcessors(resultFunction, this.postProcessors))
                }

            case _ => this.applyProcessors(expression, this.processors)
                .flatMap(this.applyProcessors(_, this.postProcessors))
        }

        this.stack.pop()
        resultExpression
    }

    def process(): Result = {
        this.stack.push(this.expression)
        this.processRecursive()
    }

    def registerProcessor(processor: LangExpression => Result): Unit = {
        this.processors = this.processors.appended(processor)
    }

    def registerPostProcessor(processor: LangExpression => Result): Unit = {
        this.postProcessors = this.postProcessors.appended(processor)
    }
}

object ExpressionProcessor {
    type Result = Writer[List[ElementError], LangExpression]
}
