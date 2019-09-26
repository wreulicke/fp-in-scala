package com.github.wreulicke.opt

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] =
    this match {
      case Some(x) => Some(f(x))
      case None => None
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    this match {
      case Some(x) => f(x)
      case None => None
    }

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case Some(x) => x
      case None => default
    }
  }

  identity()
  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this match {
      case Some(x) => Some(x)
      case None => ob
    }
  }

  def filter(f: A => Boolean): Option[A] = {
    this match {
      case Some(x) if f(x) => this
      case _ => None
    }
  }

}

case class Some[+A](get: A) extends Option[A] {

}

case object None extends Option[Nothing]
