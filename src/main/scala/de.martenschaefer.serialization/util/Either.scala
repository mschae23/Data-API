package de.martenschaefer.serialization.util

enum Either[+L, +R] {
    case Left(val value: L)
    case Right(val value: R)

    def map[B](f: R => B): Either[L, B] = this match {
        case Right(value) => Right(f(value))
        case _ => this.asInstanceOf[Either[L, B]]
    }

    def mapLeft[B](f: L => B): Either[B, R] = this match {
        case Left(value) => Left(f(value))
        case _ => this.asInstanceOf[Either[B, R]]
    }

    def flatMap[L1 >: L, R1](f: R => Either[L1, R1]): Either[L1, R1] = this match {
        case Right(value) => f(value)
        case _ => this.asInstanceOf[Either[L1, R1]]
    }

    def flatMapLeft[R1 >: R, L1](f: L => Either[L1, R1]): Either[L1, R1] = this match {
        case Left(value) => f(value)
        case _ => this.asInstanceOf[Either[L1, R1]]
    }

    def mapBoth[L1, R1](l: L => L1)(r: R => R1): Either[L1, R1] = this match {
        case Left(value) => Left(l(value))
        case Right(value) => Right(r(value))
    }

    def get[T](l: L => T)(r: R => T): T =
        this match {
            case Left(value) => l(value)
            case Right(value) => r(value)
        }

    /**
     * @throws IllegalStateException if {@code this} is not {@code Left}
     */
    def getLeft: L = this match {
        case Left(value) => value
        case _ => throw new IllegalStateException
    }

    /**
     * @throws IllegalStateException if {@code this} is not {@code Right}
     */
    def getRight: R = this match {
        case Right(value) => value
        case _ => throw new IllegalStateException
    }

    def isLeft: Boolean = this match {
        case Left(_) => true
        case _ => false
    }

    def isRight: Boolean = this match {
        case Right(_) => true
        case _ => false
    }
}
