package com.github.wreulicke.either


sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Right(a) => Right(f(a))
      case left@Left(_) => left
    }
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE,B] = {
    this match {
      case Right(a) => f(a)
      case left@Left(_) => left
    }
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this match {
      case right@Right(_) => right
      case Left(_) => b
    }
  }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for (
      aValue <- this;
      bValue <- b
    ) yield f(aValue, bValue)

}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def sequence[E, A](as: List[Either[E,A]]): Either[E, List[A]] = {
    as.foldLeft(Right(Nil):Either[E, List[A]]) { (b, a) =>
      for (
        bValue <- b;
        aValue <- a
      ) yield bValue :+ aValue
    }
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    as.foldLeft(Right(Nil):Either[E, List[B]]) { (b, a) =>
      for (
        bValue <- b;
        aValue <- f(a)
      ) yield bValue :+ aValue
    }
  }
}
