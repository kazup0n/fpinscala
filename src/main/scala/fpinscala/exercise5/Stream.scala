package fpinscala.exercise5

import scala.annotation.tailrec

trait Stream[+A] {


  def drop(n: Int): Stream[A] = {
    @tailrec
    def go(s: Stream[A], c: Int): Stream[A] = s match {
      case Cons(_, t) if c > 0 => go(t(), c - 1)
      case _ => s
    }

    go(this, n)
  }


  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if (p(h())) => Stream.cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  def take(n: Int): List[A] = {
    @tailrec
    def go(s: Stream[A], c: Int, acc: List[A]): List[A] = s match {
      case Cons(h, t) if c > 0 => go(t(), c - 1, h() :: acc)
      case _ => acc
    }

    go(this, n, List()).reverse
  }


  def toListRecursive: List[A] = this match {
    case Empty => List()
    case Cons(h, t) => h() :: t().toListRecursive
  }

  def toList: List[A] = {
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }

    go(this, List()).reverse
  }


  def existsRecursive(p: A => Boolean):Boolean = this match {
    case Cons(h, t) => p(h()) || t().existsRecursive(p)
    case _  => false
  }

  def exists(p: A => Boolean):Boolean = foldRight(identity(false))((a, z) => p(a) || z)

  def foldRight[B](z: => B)(f: (A, => B)=>B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def forAll(p: A=> Boolean):Boolean = foldRight(identity(true))((a, memo) => p(a) && memo)

  def headOption:Option[A] = foldRight(None:Option[A])( (h, _) => Some(h))

  //  def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, List())((x, xs) => Cons(f(x), xs))
  def map[B](f: A => B): Stream[B] = foldRight(identity(Empty:Stream[B]))( (a, memo) => Stream.cons(f(a), memo))

  //  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, List())((x, xs) => if (f(x)) Cons(x, xs) else xs)
  def filter(p: A=>Boolean): Stream[A] = foldRight(Empty:Stream[A])((a, memo) => if(p(a)) Stream.cons(a, memo) else memo)

  def flatMap[B](f: A=> Stream[B]) :Stream[B] = foldRight(Empty:Stream[B]) { (a, bs) =>
    f(a).foldRight(bs)((b, memo) => Stream.cons(b, memo))
  }

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

    //  def append2[A](a1: List[A], a2: List[A]): List[A] = foldLeft(reverse(a1), a2)((a, as) => Cons(a, as))
  def append[A](as:Stream[A], bs:Stream[A]):Stream[A] = as.foldRight(bs)((a, memo) =>  cons(a, memo))

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))


}