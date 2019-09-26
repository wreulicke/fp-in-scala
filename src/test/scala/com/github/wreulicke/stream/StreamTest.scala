package com.github.wreulicke.stream

import org.scalatest.{MustMatchers, WordSpec}


class StreamTest extends WordSpec with MustMatchers {

  "toList" should {
    "case 1" in {
      println(Stream(1, 2, 3).toList == List(1, 2, 3))
    }
  }

  "takeWhile" should {
    "case 1" in {
      Stream(1, 2, 3).takeWhile(_ < 2).toList mustEqual(List(1))
    }
    "case 2" in {
      Stream(1, 2, 3).takeWhile(_ < 3).toList mustEqual(List(1, 2))
    }
    "case 3" in {
      Empty.asInstanceOf[Stream[Int]].takeWhile(_ > 2).toList mustEqual(Empty.toList)
    }
  }

  "takeWhileViaFoldRight" should {
    "case 1" in {
      Stream(1, 2, 3).takeWhileViaFoldRight(_ < 2).toList mustEqual(List(1))
    }
    "case 2" in {
      Stream(1, 2, 3).takeWhileViaFoldRight(_ < 3).toList mustEqual(List(1, 2))
    }
    "case 3" in {
      Empty.asInstanceOf[Stream[Int]].takeWhileViaFoldRight(_ > 2).toList mustEqual(Empty.toList)
    }
  }

  "forAll" should {
    "Stream(1, 2, 3).forAll" should {
      "forAll(_ > 0) eq true" in {
        Stream(1, 2, 3).forAll(_ > 0) must equal (true)
      }

      "forAll(_ < 1) eq false" in {
        Stream(1, 2, 3).forAll(_ > 1) must equal (false)
      }

      "forAll(_ < 0) eq false" in {
        Stream(1, 2, 3).forAll(_ > 1) must equal (false)
      }

    }

    "Stream(1, ???).forAll" should {
      "forAll(_ < 0) eq false" in {
        Stream.cons(1, Stream(???)).forAll(_ < 1) must equal (false)
      }

      "forAll(_ < 0) shouldBe thrownBy" in {
        an [NotImplementedError] mustBe thrownBy {
          Stream.cons(1, Stream(???)).forAll(_ < 2)
        }
      }
    }

  }

  "fibs" should {
    "fibs" in {
      Stream.fibs().take(5).toList must equal(List(0, 1, 1, 2, 3))
    }
    "fibsViaUnfold" in {
      Stream.fibsViaUnfold.take(5).toList must equal(List(0, 1, 1, 2, 3))
    }
  }
}
