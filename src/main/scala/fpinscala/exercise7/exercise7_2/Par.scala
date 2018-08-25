package fpinscala.exercise7.exercise7_2

import fpinscala.exercise7.{Callable, ExecutorService, Future}

import scala.concurrent.duration.TimeUnit


object Par {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es:ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {

    override def get(timeout: Long, unit: TimeUnit): A = get

    override def cancel(evenIfRunning: Boolean): Boolean = false

    override def isDone: Boolean = true

    override def isCanceled: Boolean = false
  }


  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      override def call: A = a(es).get
    })

}

