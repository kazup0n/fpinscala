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

  def append2[A](a1: List[A], a2: List[A]): List[A] = foldLeft(reverse(a1),a2)((a, as)=>Cons(a, as))


  @tailrec
  def dropWhile[A](list: List[A])(f: A => Boolean): List[A] = list match {
    case Nil => Nil
    case Cons(x: A, xs: List[A]) => if (f(x)) dropWhile(xs)(f) else list
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

  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case Cons(_, rest: List[A]) => rest
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)(f)

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x: A, xs: List[A]) => foldLeft(xs, f(x, z))(f)
  }

  @tailrec
  def sum(ints: List[Int], _sum: Int = 0): Int = ints match {
    case Nil => _sum
    case Cons(x: Int, xs: List[Int]) => sum(xs, x + _sum)
  }

  def sum2(ns: List[Int]): Int = foldRight(ns, 0)(_ + _)
  def sum3(ns: List[Int]): Int = foldLeft(ns, 0)(_ + _)

  @tailrec
  def product(ds: List[Double], _product: Double): Double = ds match {
    case Nil => _product
    case Cons(0.0, _) => 0
    case Cons(x: Double, xs: List[Double]) => product(xs, x * _product)
  }
  def product2(ns: List[Double]): Double = foldRight(ns, 1.0)(_ * _)
  def product3(ns: List[Double]): Double = foldLeft(ns, 1.0)(_ * _)


  def length[A](as: List[A]): Int = foldRight(as, 0)((_, c) => c + 1)
  def length2[A](as: List[A]): Int = foldLeft(as, 0)((_, c) => c + 1)

  def reverse[A](as: List[A]):List[A] = foldLeft(as, List())((x:A,xs:List[A]) => Cons(x, xs))

  def flatten[A](as: List[List[A]]): List[A] = reverse(foldLeft(as, List())((x, xs)=>foldLeft(x, xs)((y, ys)=>(Cons(y, ys)))))


  def map[A, B](as: List[A])(f: A=>B):List[B] = foldRight(as, List())((x, xs)=>Cons(f(x), xs))
  def flatMap[A, B](as: List[A])(f: A=>List[B]):List[B] = flatten(foldRight(as, List())((x, xs)=>Cons(f(x), xs)))

  def filter[A](as: List[A])(f: A=>Boolean):List[A] = foldRight(as, List())((x,xs) => if(f(x)) Cons(x, xs) else xs)
  def filter2[A](as: List[A])(f: A=>Boolean):List[A] = flatMap(as)(x => if(f(x)) List(x) else Nil)

  def addOne(as: List[Int]):List[Int] = map(as)(_+1)
  def doubleToString(as: List[Double]) = map(as)(_.toString)


  def zip(xs: List[Int], ys: List[Int]):List[Int]= (xs, ys) match {
    case (Cons(x:Int, _xs:List[Int]), Cons(y:Int, _ys:List[Int])) => Cons(x+y, zip(_xs, _ys))
    case (Nil, _) => Nil
    case (_, Nil) => Nil
  }

  def zipWith[A, B](xs: List[A], ys: List[A])(f: (A, A) => B):List[B] = (xs, ys) match {
    case (Cons(x:A, _xs:List[A]), Cons(y:A, _ys:List[A])) => Cons(f(x, y), zipWith(_xs, _ys)(f))
    case (Nil, _) => Nil
    case (_, Nil) => Nil
  }

  def hasSubSequence[A](haystack: List[A], needle: List[A]):Boolean = {

    @tailrec
    def go(_haystack: List[A], _needle: List[A]):Boolean = (_haystack, _needle) match {
      case (Cons(x:A, _xs:List[A]), Cons(y:A, _ys:List[A])) => x==y && go(_xs, _ys)
      case (Nil, _) => true
      case _ => true
    }

    @tailrec
    def withTail(xs:List[A])(f: List[A]=>Boolean):Boolean = xs match {
      case Cons(_, tail:List[A]) => f(xs) || withTail(tail)(f)
      case Cons(_, Nil) => f(xs)
      case Nil => false
    }

    (haystack, needle) match {
      case (Nil, _) => false
      case (_, Nil) => true
      case _ => withTail(haystack)(go(_, needle))
    }

  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

}