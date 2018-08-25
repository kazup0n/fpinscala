package fpinscala.exercise7.nb

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}
import java.util.concurrent.atomic.AtomicReference

import fpinscala.exercise7.{Actor}

object NB {

  trait Future[A] {
    //k 計算結果を受け取るcallback, 継続??
    def apply(k: A => Unit): Unit
  }


  type Par[A] = ExecutorService => Future[A]

  def unit[A](a:A):Par[A] = _ => new Future[A] {
    override def apply(k: A => Unit): Unit = k(a)
  }

  def fork[A](a: => Par[A]): Par[A] = es => new Future[A] {
    override def apply(k: A => Unit): Unit = eval(es)(a(es)(k))
  }

  def eval(es:ExecutorService)(r: => Unit):Unit = es.submit(new Callable[Unit] {
    override def call(): Unit = r
  })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def map[A,B](p: Par[A])(f: A => B): Par[B] =
    es => new Future[B] {
      def apply(cb: B => Unit): Unit =
        p(es)(a => eval(es) {
          cb(f(a))
        })
    }

  // exercise 7.5
  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l,r) = as.splitAt(as.length/2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequence[A](as: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(as.toIndexedSeq))(_.toList)


  def choiceN[A](n: Par[Int])(choices: List[Par[A]]):Par[A] = chooser(n)(choices(_))

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = chooser(cond)(if(_) t else f)


  def flatMap[A, B](pa: Par[A])(f: A=>Par[B]):Par[B] =
    es => new Future[B] {
      override def apply(k: B => Unit): Unit = pa(es) { a =>
        f(a)(es)(k)
      }
    }

  def join[A](a: Par[Par[A]]): Par[A] =
    es => new Future[A] {
      override def apply(k: A => Unit): Unit = a(es)(_(es)(k))
    }

  def joinFlatMap[A](a: Par[Par[A]]): Par[A] = flatMap(a)(identity)


  def chooser[A, B](pa: Par[A])(choises: A => Par[B]): Par[B] =
    flatMap(pa)(a => choises(a))

//  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps.foldRight(unit(List.empty[A])) { (p, z) =>
//    //値が計算されたらリストにくっつける
//    map2(z, p)((as, a) => a +: as)
//  }

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] = sequence(as.map(asyncF(f)))

  def map2[A, B, C](p:Par[A], p2:Par[B])(f: (A, B) => C):Par[C] =
    es => new Future[C] {
      override def apply(k: C => Unit): Unit = {
        var ar: Option[A] = None
        var br: Option[B] = None

        val combiner = Actor[Either[A, B]](es) {
          case Left(a) => br match {
            case None => ar = Some(a)
            case Some(b) => eval(es)(k(f(a, b)))
          }

          case Right(b) => ar match {
            case None => br = Some(b)
            case Some(a) => eval(es)(k(f(a, b)))
          }
        }
        p(es)(a => combiner ! Left(a))
        p2(es)(b => combiner ! Right(b))
      }
    }


  def run[A](es:ExecutorService)(p:Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es) {
      a =>
        ref.set(a)
        latch.countDown
    }
    latch.await
    ref.get
  }


}

