package de.martenschaefer.data.lang

import scala.collection.immutable.ListMap
import scala.collection.mutable.Stack
import cats.catsInstancesForId
import cats.data.Writer
import de.martenschaefer.data.lang.ExpressionProcessor.*
import de.martenschaefer.data.serialization.ElementError
import de.martenschaefer.data.util.*

class ExpressionProcessor private(private var processors: List[LangExpression => Result],
                                  private var postProcessors: List[LangExpression => Result]) {
    def this() =
        this(List.empty, List.empty)

    private def applyProcessors(expression: LangExpression, processors: List[LangExpression => Result]): Result = {
        var result: Result = Writer(List.empty, expression)
        var previous: LangExpression = null

        while (result.value != previous) {
            previous = result.value

            for (processor <- processors)
                result = result.flatMap(processor)
        }

        result
    }

    def process(expression: LangExpression): Result = {
        val preProcessedExpression = this.applyProcessors(expression, this.processors)
        var warnings = List.empty[ElementError]

        val resultExpression: Result = preProcessedExpression.value match {
            case LangExpression.ArrayLiteral(values) =>
                var resultValues = List.empty[LangExpression]

                for (value <- values.reverse) {
                    val valueResult = this.process(value)
                    warnings :::= valueResult.written
                    resultValues ::= valueResult.value
                }

                val resultArray = LangExpression.ArrayLiteral(resultValues)
                Writer(warnings.reverse, resultArray).flatMap(this.applyProcessors(_, this.postProcessors))

            case LangExpression.ObjectLiteral(fields) =>
                var resultFields = ListMap.empty[LangExpression, LangExpression]

                for ((key, value) <- fields) {
                    val keyResult = this.process(key)
                    warnings :::= keyResult.written
                    val valueResult = this.process(value)
                    warnings :::= valueResult.written

                    resultFields = resultFields.updated(keyResult.value, valueResult.value)
                }

                val resultObject = LangExpression.ObjectLiteral(resultFields)
                Writer(warnings.reverse, resultObject).flatMap(this.applyProcessors(_, this.postProcessors))

            case LangExpression.FunctionCall(function, args) =>
                val functionResult = this.process(function)
                warnings :::= functionResult.written

                var resultArgs = List.empty[LangExpression]

                for (arg <- args.reverse) {
                    val argResult = this.process(arg)
                    warnings :::= argResult.written
                    resultArgs ::= argResult.value
                }

                val resultFunction = LangExpression.FunctionCall(functionResult.value, resultArgs)
                Writer(warnings.reverse, resultFunction).flatMap(this.applyProcessors(_, this.postProcessors))

            case _ => preProcessedExpression.flatMap(this.applyProcessors(_, this.processors))
                .flatMap(this.applyProcessors(_, this.postProcessors))
        }

        preProcessedExpression.flatMap(_ => resultExpression)
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
