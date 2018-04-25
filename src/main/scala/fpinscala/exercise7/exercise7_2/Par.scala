package fpinscala.exercise7.exercise7_2

import java.util.concurrent.{Callable, ExecutorService, Future}

trait Par[A] {}

case class CallablePar[A](callable:Callable[A]) extends Par[A] {
}

object CallablePar {

  implicit def toPar[A](callable: Callable[A]):Par[A] = CallablePar(callable)
  implicit def toCallable[A](pa:Par[A]): Callable[A] = pa match {
    case CallablePar(pa) => pa
  }

  def unit[A](a:A):Par[A] = new Callable[A] {
    override def call(): A = a
  }
}



object Par {
  import CallablePar.toCallable
  def unit[A](a: A):Par[A] = CallablePar.unit(a)
//  def map2[A, B, C](pa:Par[B], pb:Par[B]):Par[C]
//  def fork[A](a: => Par[A]): Par[A]
//  def lazyUnit[A](a: =>A):Par[A] =
  def run[A](es:ExecutorService)(pa:Par[A]):A = es.submit(pa).get
//
}
