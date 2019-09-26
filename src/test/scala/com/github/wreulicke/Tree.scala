package com.github.wreulicke

import com.github.wreulicke

object Tree {

  def maximum(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(x) => x
      case Branch(l, r) =>
        Tree.maximum(l) max Tree.maximum(r)
    }
  }

  def maximum2(tree: Tree[Int]): Int = tree.fold(identity)(_ max _)
}
sealed trait Tree[+A] {
  def size(): Int

  def depth[B >: A](value: B): Int

  def map[B](f: A => B): Tree[B]

  def fold[B](f: A => B)(g: (B, B) => B): B

  def map2[B](f: A => B): Tree[B] =
    fold(a => Leaf(f(a)):Tree[B])((b1, b2) => Branch(b1, b2))

  def size2() = fold(_ => 1)(_ + _)

  def depth2[B >: A](value: B): Int =
    fold(a =>
      if(a == value){
        1
      }
      else{
        -1
      }
    ) { (b1, b2) => b1 max b2 match {
      case -1 => -1
      case n => 1 + n
    }}

}

  case class Leaf[A](value: A) extends Tree[A] {
    override def size() = 1

    override def depth[B >: A](v: B): Int = if (value == v) {
      1
    } else {
      -1
    }

    override def map[B](f: A => B): Leaf[B] = Leaf(f(value))

    override def fold[B](f: A => B)(g: (B, B) => B): B = f(value)
  }

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
    override def size(): Int = 1 + left.size() + right.size()

    override def depth[B >: A](value: B): Int = {
      left.depth(value) max right.depth(value) match {
        case -1 => -1
        case n => n + 1
      }
    }

    override def map[B](f: A => B): Tree[B] = Branch(left.map(f), right.map(f))

    override def fold[B](f: A => B)(g: (B, B) => B): B = g(left.fold(f)(g), right.fold(f)(g))
  }

