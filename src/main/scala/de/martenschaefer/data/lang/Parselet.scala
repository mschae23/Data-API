package de.martenschaefer.data.lang

trait PrefixParselet {
    def parse(parser: LangParser, token: LangToken): LangParser.Result
}

trait Parselet {
    def parse(parser: LangParser, left: LangExpression, token: LangToken): LangParser.Result

    def precedence: Int
}
