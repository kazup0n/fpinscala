package fpinscala.exercise3

import scala.annotation.tailrec

sealed class List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List

object List {

  def init[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(x: A, Cons(_: A, Nil)) => List(x)
    case Cons(x, xs: List[A]) => Cons(x, init(xs))
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => Nil
    case Cons(h, t) => Cons(h, append(t, a2))
  }


  @tailrec
  def dropWhile[A](list: List[A], f: A => Boolean): List[A] = list match {
    case Nil => Nil
    case Cons(x: A, xs: List[A]) => if (f(x)) dropWhile(xs, f) else list
  }


  @tailrec
  def drop[A](list: List[A], i: Int): List[A] = list match {
    case Nil => Nil
    case Cons(_, xs: List[A]) =>
      if (i == 1) xs
      else drop(xs, i - 1)
  }


  def setHead[A](list: List[A], head: A): List[A] = list match {
    case Nil => List(head)
    case Cons(_, rest) => Cons(head, rest)
  }

  @tailrec
  def sum(ints: List[Int], _sum: Int = 0): Int = ints match {
    case Nil => _sum
    case Cons(x: Int, xs: List[Int]) => sum(xs, x + _sum)
  }

  @tailrec
  def product(ds: List[Double], _product: Double): Double = ds match {
    case Nil => _product
    case Cons(0.0, _) => 0
    case Cons(x: Double, xs: List[Double]) => product(xs, x * _product)
  }

  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case Cons(_, rest: List[A]) => rest
  }


  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

}