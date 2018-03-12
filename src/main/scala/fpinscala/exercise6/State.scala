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
  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (n, r) => {
      val next = if (n.isNegInfinity) 0
      else if (n < 0) 0
      else n
      (next, r)
    }
  }

  def double(rng: RNG): (Double, RNG) = nonNegativeInt(rng) match {
    case (n, r) => {
      ((Int.MaxValue - n) / Int.MaxValue, r)
    }
  }

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

}

