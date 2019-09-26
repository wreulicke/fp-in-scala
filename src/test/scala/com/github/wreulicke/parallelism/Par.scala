package com.github.wreulicke.parallelism

trait Par[T]{}

object Par {

  def unit[A](a: A): Par[A] = ???

  def map2[A, B, C](par1:Par[A], par2: Par[B])(f: (A, B) => C): Par[C] = ???

  def fork[A](a: => Par[A]): Par[A] = ???

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](a: Par[A]): A  = ???
}

