package de.martenschaefer.data.lang

object DefaultPrecedence {
    val OBJECT = 10

    val GROUP = 30
    val TUPLE = 35

    val SUM = 50
    val PRODUCT = 70
    val EXPONENT = 80

    val PREFIX = 120
    val POSTFIX = 130

    val FUNCTION_CALL = 200
}
