package fpinscala.exercise5

import org.scalatest.FunSuite

class StreamTest extends FunSuite {


  test("exercise 5.1 toList") {
    assert(Stream(1, 2, 3, 4, 5).toList === List(1, 2, 3, 4, 5))
    assert(Stream(1, 2, 3, 4, 5).toListRecursive === List(1, 2, 3, 4, 5))
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

  test("exercise 5.4 forAll") {
    assert(Stream(1, 3, 5, 7).forAll(_ % 2 != 0) === true)
    assert(Stream(1, 2, 3, 5, 7).forAll(_ % 2 != 0) === false)
    assert(Stream(0, 2).forAll(_ == 0) === false)
    assert(Stream(2).forAll(_ == 0) === false)
    assert(Stream(0).forAll(_ == 0) === true)
    assert(Stream[Int]().forAll(_ % 2 != 0) === true)
  }

  test("exercise 5.5 takeWhile with foldRight") {
    assert(Stream(1, 2, 3, 4).takeWhileWithFoldRight(_ % 2 != 0).toList === List(1))
    assert(Stream(1, 2, 3, 4, 5, 6, 7).takeWhileWithFoldRight(_ % 5 != 0).toList === List(1, 2, 3, 4))
  }

  test("exercise 5.6 headOption") {
    assert(Stream(1).headOption == Some(1))
    assert(Stream(1, 2).headOption == Some(1))
    assert(Stream().headOption == None)

    assert(Stream(1, 2, 3).drop(3).headOption === None)
    assert(Stream(1, 2, 3).drop(2).headOption === Some(3))
  }

  test("exercise 5.7 map") {
    assert(Stream(1, 2, 3).map(_.toString).toList === List("1", "2", "3"))
  }

  test("exercise 5.7 filter") {
    assert(Stream(1, 2, 3, 4).filter(_ % 2 == 0).toList === List(2, 4))
  }

  test("exercise 5.7 append") {
    assert(Stream.append(Stream(1, 2, 3, 4), Stream(5, 6, 7, 8)).toList === List(1, 2, 3, 4, 5, 6, 7, 8))
  }

  test("exercise 5.7 flatMap") {
    //1,2,3,4 => "1,0", "2,0", "3,0" => 1,0,2,0,3,0
    val ns = Stream(1, 2, 3).map(_.toString + ",0").flatMap(s => Stream(s.split(","): _*)).toList
    assert(ns === List("1", "0", "2", "0", "3", "0"))
  }

  test("exercise 5.8 constant"){
   assert(Stream.constant("🐈").take(5).toList == List("🐈", "🐈", "🐈" ,"🐈", "🐈"))
    assert(Stream.constant("🐟").map(_=>"🍣").take(5).toList == List("🍣", "🍣", "🍣", "🍣", "🍣"))
  }

  test("exercise 5.9 from"){
    assert(Stream.from(0).take(5).toList == List(0, 1, 3, 2, 4))
  }

  test("exercise 5.10 fibs"){
    assert(Stream.fibs.take(10).toList == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765, 10946).take(10))
  }


}
