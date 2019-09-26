package com.github.wreulicke

import com.github.wreulicke.List.foldLeft

import scala.annotation.tailrec

sealed trait List[+A] {
  def tail(): List[A]
  def setHead[B >: A](a: B): List[B]
}
case object Nil extends List[Nothing] {
  override def tail(): List[Nothing] = Nil

  override def setHead[B >: Nothing](a: B): List[B] = List(a)
}
case class Cons[+A](head: A, tail:List[A]) extends List[A] {
  override def setHead[B >: A](a: B): List[B] = Cons(a, tail)
}

object List{

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }


  def product(ds:List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  @tailrec
  def drop[A](l: List[A], n:Int): List[A] = {
    n match {
      case _ if n < 0 => ???
      case 0 => l
      case _ =>
        l match {
          case Nil => Nil
          case Cons(_, xs) => drop(xs, n - 1)
        }
    }
  }

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) =>
        if (f(x)) dropWhile(xs, f)
        else l
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]):Int = foldRight(as, 0)((_, result) => result + 1)

  // exercise 3.11
  def sum3(ns: List[Int]):Int = foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]): Double = foldLeft(ns, 1.0)(_ * _)

  def length2[A](as: List[A]):Int = foldLeft(as, 0)((result, _) => result + 1)

  def empty[A](): List[A] = Nil

  def reverse[A](as: List[A]) =
    foldLeft(as, Nil:List[A])((result, x) => Cons(x, result))

  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(as, (b:B) => b)((g, a) => b => g(f(a, b)))(z)
  }

  def append[A](as: List[A], as2:List[A]) :List[A] = foldRight(as, as2)((x, r) => Cons(x, r))

  def append2[A](as: List[A], as2:List[A]) :List[A] = foldLeft(reverse(as), as2)((b, a) => Cons(a, b))

  def flatten[A](list: List[List[A]]) = foldLeft(list, Nil:List[A])((b, a) => append(b, a))

  // exercise 3.16
  def add1(ns: List[Int]): List[Int] = {
    ns match {
      case Nil => Nil
      case Cons(x, xs) => Cons(x + 1, add1(xs))
    }
  }

  def add1_take2(ns: List[Int]): List[Int] =
    foldLeft(ns, Nil:List[Int]) {
      (b, a) => Cons(a + 1, b)
    }
    // foldRight(ns, Nil:List[Int])((a, b) => Cons(a + 1, b))

  // exercise 3.17
  def strings(ds: List[Double]): List[String] =
    foldLeft(ds, Nil:List[String]) {
      (b, a) => Cons(a.toString, b)
    }

  // exercise 3.18

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldLeft(as, Nil:List[B])((b, a) => Cons(f(a), b))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldLeft(as, Nil:List[A]) {
    (b, a) =>
      if (f(a)) {
        Cons(a, b)
      } else {
        b
      }
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    foldLeft(as, Nil:List[B]) {
      (b, a) => append(b, f(a))
    }
  }

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a =>
    if(f(a)) List(a)
    else Nil
  )

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  //

  @tailrec
  def hasSubsequence[A](sup:List[A], sub:List[A]): Boolean = {
    sub match {
      case Nil => true
      case Cons(x, xs) => {
        sup match {
          case Nil => false
          case Cons(y, ys) =>
            if (x == y) {
              hasSubsequence(ys, xs) // List(1, 4, 2, 3), List(1, 2, 3)
            } else {
              hasSubsequence(ys, sub)
            }
        }
      }
    }
  }

  @tailrec
  def hasSubsequence2[A](sup:List[A], sub:List[A]): Boolean = {
    (sup, sub) match {
      case (_, Nil) => true
      case (Cons(x, xs), Cons(y, ys)) =>
        if (x == y) {
          hasSubsequence2(xs, ys)
        } else {
          hasSubsequence2(xs, sub)
        }
      case _ => false
    }
  }

}