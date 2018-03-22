package fpinscala.exercise6

import fpinscala.exercise5.Stream
import fpinscala.exercise6.RNG.Rand
import org.scalatest.FunSuite

class StateTest extends FunSuite {

  test("exercise 6.1") {

    case class Result(rng: RNG, value: Option[Int])

    val initialRNG = SimpleRNG(10)
    val initialResult = Seq(Result(initialRNG, None))

    val firstResult = Stream.ones.take(1000).foldRight(initialResult) { (_, acc) =>
      RNG.nonNegativeInt(acc.head.rng) match {
        case (n, nextRng: SimpleRNG) => {
          assert(n >= 0)
          assert(n <= Int.MaxValue)
          Result(nextRng, Some(n)) +: acc
        }
      }
    }

    val secondResult = Stream.from(0).take(1000).foldRight(initialResult) { (_, acc) =>
      RNG.nonNegativeInt(acc.head.rng) match {
        case (n, nextRng: SimpleRNG) => {
          assert(n >= 0)
          assert(n <= Int.MaxValue)
          Result(nextRng, Some(n)) +: acc
        }
      }
    }

    assert(firstResult == secondResult)

  }

  test("exercise 6.2 double") {
    Stream.ones.take(1000).foldRight(SimpleRNG(10)) { (_, acc) =>
      RNG.double(acc) match {
        case (n, nextRng: SimpleRNG) => {
          println(n)
          assert(n >= 0)
          assert(n < 1)
          nextRng
        }
      }
    }
  }

  test("exercise 6.3 intDouble") {
    val rng = SimpleRNG(10)
    (RNG.intDouble(rng), RNG.intDouble(rng)) match {
      case ((v1, next), (v2, next2)) => {
        assert(v1 == v2)
        assert(next == next2)
      }
    }
  }

  test("exercise 6.3 doubleInt") {
    val rng = SimpleRNG(10)
    (RNG.doubleInt(rng), RNG.doubleInt(rng)) match {
      case ((v1, next), (v2, next2)) => {
        assert(v1 == v2)
        assert(next == next2)
      }
    }
  }

  test("exercise 6.3 double3") {
    val rng = SimpleRNG(10)
    (RNG.double3(rng), RNG.double3(rng)) match {
      case ((v1, next), (v2, next2)) => {
        assert(v1 == v2)
        assert(next == next2)
      }
    }
  }

  test("exercise 6.4 ints") {
    val r1 = RNG.ints(10)(SimpleRNG(10))
    val r2 = RNG.ints(10)(SimpleRNG(10))
    val r3 = RNG.ints(10)(SimpleRNG(100))
    assert(r1 == r2)
    assert(r1 != r3)
    assert(r1._1.size == 10)
    println(RNG.ints(10)(SimpleRNG(10))._1)
    println(RNG.ints(10)(SimpleRNG(10))._1)
    println(RNG.ints(10)(SimpleRNG(10))._1)
  }

  test("exercise 6.5 doubleWithMap") {
    Stream.ones.take(1000).foldRight(SimpleRNG(10)) { (_, acc) =>
      RNG.doubleWithMap(acc) match {
        case (n, nextRng: SimpleRNG) => {
          println(n)
          assert(n >= 0)
          assert(n < 1)
          nextRng
        }
      }
    }
  }

  test("exercise 6.6 map2") {
    def randIntDouble: Rand[(Int, Double)] = RNG.both(RNG.int, RNG.double)

    val rng = SimpleRNG(10)
    (randIntDouble(rng), randIntDouble(rng)) match {
      case ((v1, next), (v2, next2)) => {
        assert(v1 == v2)
        assert(next == next2)
      }
    }
  }

  test("exercise 6.7 sequence") {
    def ints(count: Int)(rng: RNG): (List[Int], RNG) = RNG.sequence2(List.fill(count)(RNG.int)).apply(rng)

    val r1 = ints(10)(SimpleRNG(10))
    val r2 = ints(10)(SimpleRNG(10))
    assert(r1 == r2)
    println(r1._1)
    println(r2._1)
  }

  test("exercise 6.8 flatMap") {
    Stream.ones.take(100).foldRight(SimpleRNG(10): RNG) { (_, rng) =>
      RNG.nonNegativeLessThanWithFlatMap(10)(rng) match {
        case (i, nextRNG) => {
          assert(i < 10)
          assert(i >= 0)
          nextRNG
        }
      }
    }
  }

  test("exercise 6.9 map with flatMap") {
    //map
    RNG.mapWithFlatMap(RNG.nonNegativeLessThan(5))(n => "🍣" * n).apply(SimpleRNG(10)) match {
      case (sushi, _) => {
        assert(sushi === "🍣" * sushi.codePointCount(0, sushi.length))
      }
    }

    //map2
    def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = RNG.map2WithFlatMap(ra, rb)((_, _))

    val rng = SimpleRNG(10)
    (both(RNG.int, RNG.double)(rng), both(RNG.int, RNG.double)(rng)) match {
      case ((v1, next), (v2, next2)) => {
        assert(v1 == v2)
        assert(next == next2)
      }
    }
  }

  test("rollDie") {
    def rollDie: Rand[Int] = RNG.nonNegativeLessThanWithFlatMap(6)

    def improvedRollDie: Rand[Int] = RNG.map(RNG.nonNegativeLessThanWithFlatMap(6))(_ + 1)

    val seedsToZero = Stream.from(0).filter(seed => rollDie(SimpleRNG(seed))._1 == 0).take(100)
    //    println(s"Seed for zero: ${seedsToZero}")
    seedsToZero.foreach(seed => assert(improvedRollDie(SimpleRNG(seed))._1 != 0))
  }


  test("exercise 6.10 flatMapWithState") {

    def nonNegativeLessThanWithFlatMap(n: Int): Rand[Int] = RNG.flatMapByState(RNG.nonNegativeInt) { i =>
      nextRNG =>
        val mod = i % n
        if (i + (n - 1) - mod >= 0) (mod, nextRNG)
        else nonNegativeLessThanWithFlatMap(n)(nextRNG)
    }

    Stream.ones.take(100).foldRight(SimpleRNG(10): RNG) { (_, rng) =>
      nonNegativeLessThanWithFlatMap(10)(rng) match {
        case (i, nextRNG) => {
          assert(i < 10)
          assert(i >= 0)
          nextRNG
        }
      }
    }
  }

  test("exercise 6.10 sequenceByState") {
    def ints(count: Int)(rng: RNG): (List[Int], RNG) = RNG.sequenceByState(List.fill(count)(RNG.int)).apply(rng)

    val r1 = ints(10)(SimpleRNG(10))
    val r2 = ints(10)(SimpleRNG(10))
    assert(r1 == r2)
    println(r1._1)
    println(r2._1)
  }

  test("exercise 6.10 mapByState, map2ByState") {
    //map
    RNG.mapByState(RNG.nonNegativeLessThan(5))(n => "🍣" * n).apply(SimpleRNG(10)) match {
      case (sushi, _) => {
        assert(sushi === "🍣" * sushi.codePointCount(0, sushi.length))
      }
    }

    //map2
    def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = RNG.map2ByState(ra, rb)((_, _))

    val rng = SimpleRNG(10)
    (both(RNG.int, RNG.double)(rng), both(RNG.int, RNG.double)(rng)) match {
      case ((v1, next), (v2, next2)) => {
        assert(v1 == v2)
        assert(next == next2)
      }
    }
  }

}
