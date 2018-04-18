package fpinscala.exercise6

import scala.annotation.tailrec


trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {

  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5deece66dl + 0xbl) & 0xffffffffffffffffl
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

}

object RNG {


  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt
  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => s(rng) match {
    case (a, rng2) => (f(a), rng2)
  }

  def mapWithFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s) { a => rng => (f(a), rng) }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => (ra(rng), rb(rng)) match {
    case ((a, next), (b, _)) => (f(a, b), next)
  }

  def map2WithFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => rng => flatMap(rb)(b => rng2 => (f(a, b), rng2))(rng))


  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    var remainFs = fs
    var next = rng

    val as = List.fill(fs.size) {
      val result = remainFs.head(rng)
      next = result._2
      remainFs = remainFs.tail
      result._1
    }
    (as, next)
  }

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    @tailrec
    def go[A](rng: RNG, fs: List[Rand[A]], acc: List[A]): (List[A], RNG) = fs match {
      case h +: tail => h(rng) match {
        case (n, next) => go(next, tail, n +: acc)
      }
      case _ => (acc, rng)
    }

    go(rng, fs, List())
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => f(rng) match {
    case (a, nextRNG) => g(a)(nextRNG)
  }


  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (n, r) => {
      val next = if (n.isNegInfinity) 0
      else if (n < 0) 0
      else n
      (next, r)
    }
  }

  def double(rng: RNG): (Double, RNG) = nonNegativeInt(rng) match {
    case (n, r) => (n.toDouble / Math.nextUp(Int.MaxValue.toDouble), r)
  }

  def doubleWithMap(rng: RNG): (Double, RNG) = map(nonNegativeInt)(_.toDouble / Int.MaxValue.toDouble).apply(rng)

  def intDouble(rng: RNG): ((Int, Double), RNG) = (nonNegativeInt(rng), double(rng)) match {
    case ((i, next), (d, _)) => {
      ((i, d), next)
    }
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = intDouble(rng) match {
    case ((i, d), next) => ((d, i), next)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = double(rng) match {
    case (d1, r1) => double(r1) match {
      case (d2, r2) => double(r2) match {
        case (d3, r3) => ((d1, d2, d3), r3)
      }
    }
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def loop(count: Int, rng: RNG, acc: List[Int]): (List[Int], RNG) = rng.nextInt match {
      case (n, next) => if (count > 0) loop(count - 1, next, n +: acc) else (acc, rng)
    }

    loop(count, rng, List())
  }


  def nonNegativeLessThan(n: Int): Rand[Int] = rng => nonNegativeInt(rng) match {
    case (i, rng2) => {
      val mod = i % n
      if (i + (n - 1) - mod >= 0) (mod, rng2)
      else nonNegativeLessThan(n)(rng)
    }
  }

  def nonNegativeLessThanWithFlatMap(n: Int): Rand[Int] = flatMap(nonNegativeInt) { i =>
    nextRNG =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) (mod, nextRNG)
      else nonNegativeLessThanWithFlatMap(n)(nextRNG)
  }
}

