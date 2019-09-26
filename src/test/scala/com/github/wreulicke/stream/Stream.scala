package com.github.wreulicke.stream

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(head _, tail _)
  }
  def empty[A]: Stream[A] = Empty
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = {
    lazy val r:Stream[A] = cons(a, r)
    r
  }

  def from(n: Int): Stream[Int] = cons(n + 1, from(n + 1))

  def fibs(): Stream[Int] = {
    def f(a: Int, b:Int): Stream[Int] = cons(a, f(b, a + b))
    f(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, b)) => cons(a, unfold(b)(f))
      case None => Empty
    }
  }

  def fibsViaUnfold: Stream[Int] = {
    unfold((0, 1)) {
      case (a, b) => Some(a, (b, a + b))
    }
  }

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(it => Some((it, it + 1)))

  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(it => Some(it, it))



}

sealed trait Stream[+A] {
  def toList: List[A]

  def forAll(p: A => Boolean): Boolean
    = foldRight(true)((a, b) => p(a) && b)


  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def take(n: Int): Stream[A] = if (n <= 0) {
    Empty
  } else {
    this match {
      case Empty => Empty
      case Cons(x, xs) =>
        Stream.cons(x(), xs().take(n-1))
    }
  }

  def skip(n: Int): Stream[A] = if (n <= 0) {
    this
  } else {
    this match {
      case Empty => Empty
      case Cons(_, xs) => xs().skip(n -1)
    }
  }

  def headOption:Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def headOptionViaFoldRight: Option[A] = {
    foldRight(Nil.asInstanceOf[Option[A]]) { (a, _) =>
      Some(a)
    }
  }


  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(x, xs) if p(x()) => Stream.cons(x(), xs().takeWhile(p))
      case _ => Empty
    }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = {
    foldRight(Empty: Stream[A]) { (a, b) =>
      if (p(a))
        Stream.cons(a, b)
      else
        b
    }
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    Stream.unfold(this, s2) {
      case (Empty, Empty) => None
      case (Cons(a, ax), Empty) => Some((Some(a()), None) -> (ax(), Empty))
      case (Empty, Cons(a, ax)) => Some(((None, Some(a())), (Empty, ax())))
      case (Cons(a, ax), Cons(b, bx)) => Some(((Some(a()), Some(b())), (ax(), bx())))
    }
  }
}

case object Empty extends Stream[Nothing] {
  override def toList: List[Nothing] = Nil
}
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
  lazy val head:A = h()
  lazy val tail: Stream[A] = t()
  override def toList: List[A] = head :: tail.toList
}
