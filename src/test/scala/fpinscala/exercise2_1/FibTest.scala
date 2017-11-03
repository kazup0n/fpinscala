package fpinscala.exercise2_1

import org.scalatest.FunSuite

class FibTest extends FunSuite {

  test("fib(1) = 0") {
    assert(fib(1) == 0)
  }

  test("fib(2) = 1") {
    assert(fib(2) == 1)
  }

  test("fib(3) = 1") {
    assert(fib(2) == 1)
  }

  test("fib(4) = 2") {
    assert(fib(2) == 1)
  }

  test("fib(n)") {
    Seq(0, 1, 1, 2, 3, 5, 8, 13).zipWithIndex.foreach(_ match {
      case (n: Int, i: Int) => assert(fib(i + 1) == n)
    })
  }

  test("fib(35) = 5702887") {
    assert(fib(35) == 5702887)
  }


}
