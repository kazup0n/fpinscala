package fpinscala.exercise5

import org.scalatest.FunSuite

class StreamTest extends FunSuite {


  test("exercise 5.1 toList") {
    assert(Stream(1, 2, 3, 4, 5).toList === List(1, 2, 3, 4, 5))
    assert(Stream(1, 2, 3, 4, 5).toListResursive === List(1, 2, 3, 4, 5))
  }

  test("exercise 5.2 take") {
    assert(Stream(1, 2, 3, 4, 5).take(5) === List(1, 2, 3, 4, 5))
    assert(Stream(1, 2, 3, 4, 5).take(2) === List(1, 2))
    assert(Stream(1, 2, 3, 4, 5).take(6) === List(1, 2, 3, 4, 5))
  }

  test("exercise 5.2 drop") {
    assert(Stream(1, 2, 3, 4).drop(2).toList === List(3, 4))
    assert(Stream(1, 2, 3, 4).drop(10).toList === List())
  }

  test("exercise 5.3 takeWhile") {
    assert(Stream(1, 2, 3, 4).takeWhile(_ % 2 != 0).toList === List(1))
    assert(Stream(1, 2, 3, 4, 5, 6, 7).takeWhile(_ % 5 != 0).toList === List(1, 2, 3, 4))
  }

}
