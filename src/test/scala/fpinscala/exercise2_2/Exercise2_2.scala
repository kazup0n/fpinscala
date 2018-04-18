package fpinscala.exercise2_2

import org.scalatest.FunSuite

import scala.util.Random

class Exercise2_2 extends FunSuite {

  def sortInt(a: Int, b: Int): Boolean = b > a

  def sortString(a: String, b: String): Boolean = b > a


  test("sorted number") {
    assert(isSorted(Array(1, 2, 3, 4, 5), sortInt))
  }

  test("unsorted number") {
    assert(isSorted(Array(1, 3, 2, 4, 5), sortInt) == false)
  }

  test("Shuffled array") {
    val random = Random.shuffle(Range(0, 500, 2).toList).toArray
    assert(isSorted(random, sortInt) == false)
  }

  test("sorted strings") {
    assert(isSorted(Array("a", "b", "c", "d"), sortString))
  }

  test("unsorted strings") {
    val original = Array("b", "a", "d", "c")
    assert(isSorted(original, sortString) == false)
  }

}
