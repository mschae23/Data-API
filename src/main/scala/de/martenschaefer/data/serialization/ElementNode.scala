package de.martenschaefer.data.serialization

enum ElementNode {
    case Name(val name: String)
    case Index(val index: Int)

    override def toString: String = this match {
        case Name(name) => "." + name
        case Index(index) => "[" + index + "]"
    }
}
