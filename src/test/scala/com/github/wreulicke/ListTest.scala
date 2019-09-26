package com.github.wreulicke

import org.scalatest.FreeSpec

class ListTest extends FreeSpec {

  "Exercise 3-1" - {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
    }
    assert(x == 3)
  }

  "Ex 3-2 List.tail" - {
    "Cons(x, Nil).tail be Nil" in {
      assert(List(1).tail == Nil)
    }

    "Cons(x, Cons(...)).tail be Cons(...)" in {
      assert(List(1, 2, 3).tail == List(2, 3))
    }

    "Nil.tail be Nil" in {
      assert(List(1, 2, 3).tail == List(2, 3))
    }
  }

  "Ex 3-3 List.setHead" - {
    "Nil.setHead be Cons" in {
      assert(Nil.setHead(2) == List(2))
    }

    "Cons(x, xs).setHead(a) be Cons(a, xs)" in {
      assert(List(1, 2, 3).setHead(2) == List(2, 2, 3))
    }
  }

  "Ex 3-4 List.drop" - {
    "drop(Nil, 1) => Nil" in {
      assert(List.drop(Nil, 1) == Nil)
    }
    "drop(List(1), 0) => List(1)" in {
      assert(List.drop(List(1), 0) == List(1))
    }
    "drop(List(1, 2, 3), 1) => List(2, 3)" in {
      assert(List.drop(List(1, 2, 3), 1) == List(2, 3))
    }
    "drop(_, negative) => thrown ???" in {
      assertThrows[NotImplementedError](List.drop(Nil, -1))
    }
  }

  "Ex 3-5 List.dropWhile" - {
    "dropWhile(Nil, _ => true) => Nil" in {
      assert(List.dropWhile(Nil, (_: Nothing) => true) == Nil)
    }
    "dropWhile(List(1, 2, 3), n => 1) => List(2, 3)" in {
      assert(List.dropWhile(List(1, 2, 3), (_: Int) == 1) == List(2, 3))
    }
    "dropWhile(List(1, 2, 3), _ => false) => List(1, 2, 3)" in {
      assert(List.dropWhile(List(1, 2, 3), (_: Int) => false) == List(1, 2, 3))
    }
  }

  "Ex 3-6 List.init" - {
    "init(Nil, _ => true) => Nil" in {
      assert(List.init(Nil) == Nil)
    }
    "init(List(1, 2, 3)) => List(2, 3)" in {
      assert(List.init(List(1, 2, 3)) == List(1, 2))
    }
  }

  "Ex 3-10 List.foldLeft" - {
    "Nil" in {
      assert(List.foldLeft(Nil: List[Int], 2)(_ + _) == 2)
    }
    "Cons" in {
      assert(List.foldLeft(Cons(3, Nil), 2)(_ + _) == 5)
    }
  }

  "Ex 3-11 sum, product, length" - {
    "sum" in {
      assert(List.sum3(Cons(1, Nil)) == 1)
      assert(List.sum3(Cons(3, Cons(2, Nil))) == 5)
      assert(List.sum3(Nil) == 0)
    }
    "product" in {
      assert(List.product3(Cons(1.1, Nil)) == 1.1)
      assert(List.product3(Cons(2.0, Cons(1.1, Nil))) == 2.2)
      assert(List.product3(Nil) == 1.0)
    }
    "length" in {
      val list1 = Cons(1, Nil)
      val list2 = Cons(2, list1)
      val list3 = Cons(3, list2)
      assert(List.length2(list1) == 1)
      assert(List.length2(list2) == 2)
      assert(List.length2(list3) == 3)
    }
  }

  "Ex 3-12 List.reverse" - {
    "Nil" in {
      assert(List.reverse(List(1, 2)) == List(2, 1))
    }
  }

  "Ex 3-15" - {
    "flatten" in {
      assert(List.flatten(List(List(1, 2), List(3, 4))) == List(1, 2, 3, 4))
    }
  }

  "Ex 3-16" - {
    "append2" in {
      assert(List.append2(List(1, 2), List(3, 4)) == List(1, 2, 3, 4))
    }
  }

  "Ex 3-20" - {
    "flatMap" in {
      assert(List.flatMap(List(1, 2, 3))(i => List(i, i)) == List(1, 1, 2, 2, 3, 3))
    }
  }

  "Ex 3-21" - {
    "filter" in {
      assert(List.filter(List(1, 2, 3))(_ == 2) == List(2))
    }
    "filterViaFlatMap" in {
      assert(List.filterViaFlatMap(List(1, 2, 3))(_ == 2) == List(2))
    }
  }

  "Ex 3-24" - {
    "hasSubsequence case1" in {
      assert(List.hasSubsequence(List(1, 2, 3), List(2, 3)))
    }

    "hasSubsequence case2" in {
      assert(List.hasSubsequence(List(1, 2, 3, 5), List(2, 3)))
    }

    "hasSubsequence case3" in {
      assert(List.hasSubsequence(List(1, 4, 2, 3), List(1, 2, 3)))
    }

    "hasSubsequence2 case1" in {
      assert(List.hasSubsequence2(List(1, 2, 3), List(2, 3)))
    }

    "hasSubsequence2 case2" in {
      assert(List.hasSubsequence2(List(1, 2, 3, 5), List(2, 3)))
    }
  }

}
