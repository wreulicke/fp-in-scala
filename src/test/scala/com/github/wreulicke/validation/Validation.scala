package com.github.wreulicke.validation


sealed trait Validation[+E, +A] {

  def map[B](f: A => B): Validation[E, B] = {
    this match {
      case Right(a) => Right(f(a))
      case left@Left(_) => left
    }
  }

  def flatMap[EE >: E, B](f: A=> Validation[EE, B]): Validation[EE,B] ={
    this match {
      case Right(a) => f(a)
      case left@Left(_) => left
    }
  }

  def orElse[EE >: E, B >: A](b: => Validation[EE, B]): Validation[EE, B] = {
    this match {
      case right@Right(_) => right
      case Left(_) => b
    }

//    (this, b) match {
//      case (a@Right(_), _) => a
//      case (Left(as), Left(bs)) => Left(as ::: bs)
//      case (_, b@Right(_)) => b
//    }
  }

  def map2[EE >: E, B, C](b: Validation[EE, B])(f: (A, B) => C): Validation[EE, C] =
    for (
      aValue <- this;
      bValue <- b
    ) yield f(aValue, bValue)

  def sequence[E, A](as: List[Validation[E,A]]): Validation[E, List[A]] = {
    as.foldLeft(Right(Nil):Validation[E, List[A]]) { (b, a) =>
      for (
        bValue <- b;
        aValue <- a
      ) yield bValue :+ aValue
    }
  }

  def traverse[E, A, B](as: List[A])(f: A => Validation[E, B]): Validation[E, List[B]] = {
    as.foldLeft(Right(Nil):Validation[E, List[B]]) { (b, a) =>
      for (
        bValue <- b;
        aValue <- f(a)
      ) yield bValue :+ aValue
    }
  }
}

case class Left[+E](values: List[E]) extends Validation[E, Nothing]

case class Right[+A](value: A) extends Validation[Nothing, A]
