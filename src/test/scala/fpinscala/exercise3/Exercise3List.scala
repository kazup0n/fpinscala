package fpinscala.exercise3

import org.scalatest.FunSuite

class Exercise3List extends FunSuite {

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
    assert(List.dropWhile(List(1, 3, 5, 2))(n => n % 2 != 0) == List(2))
  }

  test("exercise 3.6 - init") {
    assert(List.init(List(1, 2, 3, 4)) == List(1, 2, 3))
  }

  test("exercise 3.9 - length") {
    assert(List.length(List(1, 2, 3, 4, 5)) == 5)
  }

  test("exercise 3.11 - sum, product, length with foldLeft") {
    assert(List.sum3(List(1, 2, 3)) == 6)
    assert(List.product3(List(1, 2, 3)) == 6.0)
    assert(List.length2(List(1, 2, 3)) == 3)
  }

  test("exercise 3.12 - reverse") {
    assert(List.reverse(List(1, 2, 3)) == List(3, 2, 1))
  }

  test("exercise 3.13 - append") {
    assert(List.append2(List(1, 2, 3), List(4, 5, 6)) == List(1, 2, 3, 4, 5, 6))
  }

  test("exercise 3.15 - flatten") {
    val listOfList = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))
    assert(List.flatten(listOfList) == List(1, 2, 3, 4, 5, 6, 7, 8, 9))
  }

  test("exercise 3.16 - addOne") {
    assert(List.addOne(List(1, 2, 3)) == List(2, 3, 4))
  }

  test("exercise 3.17 - toString") {
    assert(List.doubleToString(List(1.0, 2.0, 3.0)) == List("1.0", "2.0", "3.0"))
  }

  test("exercise 3.18 - map") {
    assert(List.map(List(1.0, 2.0, 3.0))(x => x.toString) == List("1.0", "2.0", "3.0"))
  }

  test("exercise 3.19 - filter") {
    assert(List.filter(List(1, 2, 3, 4, 5, 6))(_ % 2 == 0) == List(2, 4, 6))
  }

  test("exercise 3.20 - flatMap") {
    assert(List.flatMap(List(1, 2, 3))(i => List(i, i)) == List(1, 1, 2, 2, 3, 3))
  }

  test("exercise 3.21 - filter2") {
    assert(List.filter2(List(1, 2, 3, 4, 5, 6))(_ % 2 == 0) == List(2, 4, 6))
  }

  test("exercise 3.22 - zip") {
    assert(List.zip(List(1, 2, 3), List(4, 5, 6)) === List(5, 7, 9))
  }

  test("exercise 3.23 - zipWith") {
    assert(List.zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _) === List(5, 7, 9))
  }

  test("exercise 3.24 - hasSubSequence") {
    assert(List.hasSubSequence(List(1, 2, 3, 4, 5, 6), List(3, 4, 5)) === true)
    assert(List.hasSubSequence(List(1, 2, 3, 4, 5, 6), List(1, 2, 3)) === true)
    assert(List.hasSubSequence(List(1, 2, 3, 4, 5, 6), List(6)) === true)
    assert(List.hasSubSequence(List(1, 2, 3, 4, 5, 6), Nil) === true)

    assert(List.hasSubSequence(List(1, 2, 3, 4, 5, 6), List(3, 5, 6)) === false)
    assert(List.hasSubSequence(List(1), List(3, 5, 6)) === false)
    assert(List.hasSubSequence(Nil, List(3, 5, 6)) === false)
  }

}
