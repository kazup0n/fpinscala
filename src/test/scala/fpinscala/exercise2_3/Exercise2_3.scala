package fpinscala.exercise2_3

import org.scalatest.FunSuite

class Exercise2_3 extends FunSuite {


  test("curry") {
    assert(curry((a: Int, b: Int) => a + b)(100)(10) == 110)
    assert(curry((a: String, b: String) => a + b)("Hello, ")("World") == "Hello, World")
  }

}
