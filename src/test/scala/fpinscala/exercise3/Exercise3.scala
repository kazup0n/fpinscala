package fpinscala.exercise3

import org.scalatest.FunSuite

class Exercise3 extends FunSuite {

  test("exercise 3.1 - test match") {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x: Int, Cons(y: Int, Cons(3, Cons(4, _)))) => x + y
      case _ => 101
    }
    assert(x == 3)
  }

  test("exercise 3.2 - tail") {
    assert(List.tail(List(1, 2, 3)) == List(2, 3))
    assert(List.tail(List(1, 2)) != List(2, 3))

    assert(List.tail(List(1)) == Nil)
    assert(List.tail(Nil) == Nil)
  }

  test("exercise 3.3 - setHead") {
    assert(List.setHead(List(1, 2, 3), 0) == List(0, 2, 3))
    assert(List.setHead(List(1), 0) == List(0))
    assert(List.setHead(Nil, 0) == List(0))

    assert(List.setHead(List("A", "B", "C"), "Z") == List("Z", "B", "C"))
    assert(List.setHead(List("A"), "Z") == List("Z"))
    assert(List.setHead(Nil, "Z") == List("Z"))
  }

  test("exercise 3.4 - drop") {
    assert(List.drop(List(1, 2, 3, 4), 2) == List(3, 4))
    assert(List.drop(List(1, 2, 3, 4), 4) == Nil)
    assert(List.drop(Nil, 5) == Nil)
  }

  test("exercise 3.5 - dropWhile") {
    def odd(n: Int): Boolean = (n % 2 != 0)

    assert(List.dropWhile(List(1, 3, 5, 2), odd) == List(2))
  }

  test("exercise 3.6 - init") {
    assert(List.init(List(1, 2, 3, 4)) == List(1, 2, 3))
  }


}
