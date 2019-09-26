package com.github.wreulicke

import org.scalatest.FreeSpec

class TreeTest extends FreeSpec {

  "Ex 3.25 size" - {

    "case 1" in {
      assert(Leaf(2).size() == 1)
    }

    "case 2" in {
      assert(Branch(Leaf(1), Leaf(1)).size() == 3)
    }

    "case 3" in {
      assert(Branch(Leaf(1), Branch(Leaf(1), Leaf(1))).size() == 5)
    }

  }

  "Ex 3.26 maximum" - {


  }

}
