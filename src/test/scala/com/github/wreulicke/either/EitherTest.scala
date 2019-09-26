package com.github.wreulicke.either

import org.scalatest.FreeSpec

class EitherTest extends FreeSpec {

  "Ex 4.6" - {
    "map case 1" in {
      assert(Right(2).map(_ + 2) == Right(4))
    }
    "map case 2" in {
      val actual: Either[Double, Int] = Left(2.0)
      assert(actual.map(_ + 2) == Left(2.0))
    }

    "flatMap case 1" in {
      val actual: Either[Double, Int] = Left(2.0)
      assert(actual.flatMap(v => Left(1.0 + v)) == Left(2.0))
    }

    "flatMap case 2" in {
      val actual: Either[Double, Int] = Left(2.0)
      assert(actual.flatMap(v => Right(2 + v)) == Left(2.0))
    }

    "flatMap case 3" in {
      val actual: Either[Int, Double] = Right(2.0)
      assert(actual.flatMap(v => Right(2 + v)) == Right(4.0))
    }
  }

  "Ex 4.7" - {
    "sequence case 1" in {
      val actual = Either.sequence(List(Right(1), Right(2)))
      assert(actual == Right(List(1, 2)))
    }
    "sequence case 2" in {
      val actual = Either.sequence(List(Left("fucking"), Right(2)))
      assert(actual == Left("fucking"))
    }
    "sequence case 3" in {
      val actual = Either.sequence(List(Left("fucking1"), Right(2), Left("fucking3")))
      assert(actual == Left("fucking1"))
    }

    "traverse case 1" in {
      val actual = Either.traverse(List(1, 2))(n => Right(n + 3))
      assert(actual == Right(List(4, 5)))
    }
    "traverse case 2" in {
      val actual = Either.traverse(List(Left("fucking"), Right(2)))(v => v)
      assert(actual == Left("fucking"))
    }
    "traverse case 3" in {
      val actual = Either.traverse(List(Left("fucking1"), Right(2), Left("fucking3")))(v => {
        println(v)
        v
      })
      assert(actual == Left("fucking1"))
    }
  }

}
