package de.martenschaefer.data.lang.impl

case class LexerString(private val internalString: String, private var pos: Int) {
    def this(string: String) = this(string, 0)

    def next(): Option[Char] = {
        if (this.pos >= this.internalString.length())
            return None

        this.pos += 1
        Some(this.internalString.charAt(this.pos - 1))
    }

    def peek: Option[Char] = {
        if (this.pos >= this.internalString.length())
            return None

        Some(this.internalString.charAt(this.pos))
    }
}
