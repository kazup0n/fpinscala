package fpinscala.exercise7

import java.util.concurrent.TimeUnit

import scala.concurrent.duration.TimeUnit


object Par {

  type Par[A] = ExecutorService => Future[A]

  def noop[A]: Par[A] = null

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def unit[A](a: => A): Par[A] = (_: ExecutorService) => UnitFuture(a)

  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
    override def call: A = a(es).get
  })


  // exercise 7.1
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => (a(es), b(es)) match {
      case (af, bf) => UnitFuture(f(af.get, bf.get))
    }

  // exercise 7.3
  def map2Timeout[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => (a(es), b(es)) match {
      case (af, bf) => Map2Future(af, bf)(f)
    }

  //exercise 7.4
  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))

  // exercise 7.5
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps.foldRight(Par.unit(List.empty[A])) { (p, z) =>
    //値が計算されたらリストにくっつける
    map2(z, p)((as, a) => a +: as)
  }

  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] = fork(sequence(as.map(asyncF(f))))

  // exercise 7.6
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
    val fas = as.map(lazyUnit(_))
    filteredSeq(fas)(f)
  }


  def filteredSeq[A](ps: List[Par[A]])(f: A => Boolean): Par[List[A]] = ps.foldRight(Par.unit(List.empty[A])) { (p, z) =>
    //値が計算されたらリストにくっつける
    map2(z, p)((as, a) => if (f(a)) a +: as else as)
  }

  def parFilter2[A](as: List[A])(f: A => Boolean):Par[List[A]] = map(parMap(as) { a =>
    if(f(a)) List(a) else List.empty
  })(_.flatten)


  def sum(ints: IndexedSeq[Int]): Par[Int] = map(parMap(ints.toList)(identity))(_.sum)
  def max(ints: IndexedSeq[Int]): Par[Int] = map(sortPar(parMap(ints.toList)(identity)))(_.head)
  def wordCount(paragraphs:List[String]):Par[List[Int]] = parMap(paragraphs)(_.split(" ").length)
  def count[A](items:List[A])(f:A => Int):Par[List[Int]] = parMap(items)(f)

  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] =
    map2(map2(a, b)((_,_)), c) { (ab, c) =>
      f(ab._1, ab._2, c)
    }

  def map4[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: (A, B, C, D) => E): Par[E] =
    map2(map3(a, b, c)((_,_,_)), d){ (abc, d) =>
      f(abc._1, abc._2, abc._3, d)
    }

  def map5[A, B, C, D, E, F](a: Par[A], b: Par[B], c: Par[C], d: Par[D], e: Par[E])(f: (A, B, C, D, E) => F): Par[F] =
    map2(map4(a,b,c,d)((_,_,_,_)), e) { (abcd, e) =>
      f(abcd._1, abcd._2, abcd._3, abcd._4, e)
    }

  def equal[A](e: ExecutorService)(p: Par[A], p2:Par[A]): Boolean = p(e).get == p2(2).get





  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  // exercise 7.3
  case class Map2Future[A, B, C](a: Future[A], b: Future[B])(f: (A, B) => C) extends Future[C] {

    override def get: C = f(a.get, b.get)

    override def get(timeout: Long, unit: TimeUnit): C = {
      //aが終わってから残り時間でbを実行するけど、並列になってないのが納得いかない
      val time = unit.convert(timeout, TimeUnit.NANOSECONDS)
      val start = System.currentTimeMillis
      val ra = a.get(time, TimeUnit.NANOSECONDS)

      val remain = System.currentTimeMillis - start
      val rb = b.get(remain, TimeUnit.NANOSECONDS)
      f(ra, rb)
    }

    override def cancel(evenIfRunning: Boolean): Boolean = a.cancel(evenIfRunning) || b.cancel(evenIfRunning)

    override def isDone: Boolean = a.isDone && b.isDone

    override def isCanceled: Boolean = a.isCanceled || b.isCanceled
  }

  private case class UnitFuture[A](get: A) extends Future[A] {

    override def get(timeout: Long, unit: TimeUnit): A = get

    override def cancel(evenIfRunning: Boolean): Boolean = false

    override def isDone: Boolean = true

    override def isCanceled: Boolean = false
  }

}


abstract class ExecutorService {
  def submit[A](a: Callable[A]): Future[A]
}

trait Callable[A] {
  def call: A
}

trait Future[A] {
  def get: A

  def get(timeout: Long, unit: TimeUnit): A

  def cancel(evenIfRunning: Boolean): Boolean

  def isDone: Boolean

  def isCanceled: Boolean

}
