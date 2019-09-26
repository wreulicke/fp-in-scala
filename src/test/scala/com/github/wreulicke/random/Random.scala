package com.github.wreulicke.random


trait RNG {

  def nextInt: (Int, RNG)

}

case class SimpleRNG(seed: Long) extends RNG {

  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE666DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

}

object Random {
  type Rand[+A] = RNG => (A, RNG)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def int(rng: RNG): (Int, RNG) = rng.nextInt

  def double(rng: RNG): (Double, RNG) = {
    val (n, nextRng) = nonNegativeInt(rng)
    (n.toDouble / (Int.MaxValue + 1), nextRng)
  }

  // Ex 6.5
  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt)((_.toDouble / (Int.MaxValue + 1)))

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng2) = rng.nextInt
    (n.abs, rng2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (int, rng2) = rng.nextInt
    val (double, rng3) = Random.double(rng2)
    ((int, double), rng3)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (double1, rng2) = Random.double(rng)
    val (double2, rng3) = Random.double(rng2)
    val (double3, rng4) = Random.double(rng3)
    ((double1, double2, double3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    (0 until count).foldLeft((Nil.asInstanceOf[List[Int]], rng)) {
      case ((list, rng), _) =>
        val (n, rng2) = rng.nextInt
        (n +: list, rng2)
    }
  }

  def map2[A, B, C](ra: Rand[A], rb:Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b) , rng3)
    }
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = {
    map2(ra, rb) {
      (_, _)
    }
  }

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng => {
      fs.foldLeft((Nil.asInstanceOf[List[A]], rng)) {
        (as, rand) =>
          val (a, rng2) = rand(as._2)
          ((a +: as._1), rng2)
      }
    }
  }
}


