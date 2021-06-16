package de.martenschaefer.data.util

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

    def flatMapBoth[L1, R1](l: L => Either[L1, R1])(r: R => Either[L1, R1]): Either[L1, R1] = this match {
        case Left(value) => l(value)
        case Right(value) => r(value)
    }

    def get[T](l: L => T)(r: R => T): T =
        this match {
            case Left(value) => l(value)
            case Right(value) => r(value)
        }

    def orElse[R1 >: R](alternative: => R1): Either[L, R1] = this match {
        case Left(_) => Right(alternative)
        case _ => this
    }

    def flatOrElse[L1 >: L, R1](alternative: => Either[L1, R1]): Either[L1, R1] = this match {
        case Left(_) => alternative
        case _ => this.asInstanceOf[Either[L1, R1]]
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

import scala.annotation.tailrec
import cats.Monad

object Either {
    given[L]: Monad[[R] =>> Either[L, R]] with {
        override def pure[R](value: R): Either[L, R] = Right(value)

        override def flatMap[R, R1](either: Either[L, R])(f: R => Either[L, R1]): Either[L, R1] = either.flatMap(f)

        override def tailRecM[A, B](a: A)(f: A => Either[L, scala.Either[A, B]]): Either[L, B] = {
            @tailrec
            def loop(aa: A): Either[L, B] = f(a) match {
                case Right(either) => either match {
                    case scala.Left(a1) => loop(a1)
                    case scala.Right(b) => Right(b)
                }

                case Left(value) => Left(value)
            }

            loop(a)
        }
    }
}
