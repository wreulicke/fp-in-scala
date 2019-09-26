package com.github.wreulicke.opt

import org.scalatest.FreeSpec

class OptionTest extends FreeSpec {


  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    // mean(xs) flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
    for (
      m <- mean(xs);
      v <- mean(xs.map(x => math.pow(x - m, 2)))
    ) yield v
  }

  def map2[A, B, C](aOpt: Option[A], bOpt: Option[B])(f: (A, B) => C): Option[C] =
    for (
      a <- aOpt;
      b <- bOpt
    ) yield f(a, b)

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldLeft(Some(Nil): Option[List[A]])(
      // (bOpt, listOpt) => bOpt.flatMap(b => listOpt.map(list => b :: list)))
      (listOpt, bOpt) => for (
        b <- bOpt;
        list <- listOpt
      ) yield list :+ b
    )
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldLeft(Some(Nil): Option[List[B]])(
      // (a, listOpt) => f(a).flatMap(b => listOpt.map(list => b :: list)))
      (listOpt, a) => for (
        b <- f(a);
        list <- listOpt
      ) yield list :+ b)
  }

  "Ex 4.1 " - {

    "map case 1" in {
      assert(Some(1).map(_ + 2) == Some(3))
    }

    "map case 2" in {
      assert(None.map(_ => throw new RuntimeException) == None)
    }

    "flatMap case 1" in {
      assert(Some(1).flatMap(v => Some(v + 2)) == Some(3))
    }

    "flatMap case 2" in {
      assert(None.flatMap(_ => throw new RuntimeException) == None)
    }

    "flatMap case 3" in {
      assert(Some(1).flatMap(_ => None) == None)
    }


    "getOrElse case 1" in {
      assert(Some(1).getOrElse(3) == 1)
    }

    "getOrElse case 2" in {
      assert(None.getOrElse(3) == 3)
    }

    "getOrElse case 3" in {
      assert(
        Some(1).getOrElse({
          throw new RuntimeException
        }) == 1)

    }
    "getOrElse case 4" in {
      assertThrows[RuntimeException] {
        None.getOrElse({
          throw new RuntimeException
        })
      }
    }

  }

  "Ex 4.2 variance" - {

    "1, 2, 3 の variance" in {
      assert(mean(Seq(1, 2, 3)) == Some(2))
      assert(variance(Seq(1, 2, 3)) == Some(2.0 / 3))
    }

  }

  "Ex 4.3 map2" - {

    "map2 case 1" in {
      assert(map2(Some(1), None)((x, y) => x + y) == None)
    }

    "map2 case 2" in {
      assert(map2(Some(1), Some(2))((x, y) => x + y) == Some(3))
    }
  }


  "Ex 4.4 sequence" - {

    "sequence case 1" in {
      assert(sequence(List(Some(1), Some(2), Some(3))) == Some(List(1, 2, 3)))
    }

    "sequence case 2" in {
      assert(sequence(List(Some(1), None)) == None)
    }
  }

  "Ex 4.5 traverse" - {

    "traverse case 1" in {

      assert(traverse(List(1, 2, 3))(_ => None) == None)

    }

    "traverse case 2" in {

      assert(traverse(List(1, 2, 3))(Some(_)) == Some(List(1, 2, 3)))

    }

    "traverse case 3" in {

      assert(traverse(List(1, 2, 3))(
        n => if (n % 2 == 0) None
        else Some(n)
      ) == None)

    }
  }

}
