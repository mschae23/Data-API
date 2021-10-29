package de.martenschaefer.data.lang

trait PrefixParselet {
    def parse(parser: LangParser, token: LangToken): Option[LangExpression]
}

trait Parselet {
    def parse(parser: LangParser, left: LangExpression, token: LangToken): Option[LangExpression]

    def precedence: Int
}
