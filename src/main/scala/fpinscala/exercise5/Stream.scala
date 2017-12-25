package fpinscala.exercise5

import scala.annotation.tailrec

trait Stream[+A] {


  def drop(n: Int):Stream[A] = {
    @tailrec
    def go(s: Stream[A], c:Int): Stream[A] = s match {
      case Cons(_, t) if c > 0 => go(t(), c-1)
      case _ => s
    }
    go(this, n)
  }


  def takeWhile(p: A=>Boolean): Stream[A] = this match {
    case Cons(h, t) if(p(h())) => Stream.cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  def take(n:Int): List[A] = {
    @tailrec
    def go(s:Stream[A], c:Int, acc:List[A]):List[A] = s match {
      case Cons(h, t) if c > 0 => go(t(), c-1, h()::acc)
      case _ => acc
    }
    go(this, n, List()).reverse
  }



  def toListResursive:List[A] = this match {
    case Empty => List()
    case Cons(h, t) => h()::t().toListResursive
  }

  def toList:List[A] = {
    def go(s: Stream[A], acc:List[A]):List[A] = s match {
      case Cons(h, t) => go(t(), h()::acc)
      case _ => acc
    }
    go(this, List()).reverse
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

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))


}