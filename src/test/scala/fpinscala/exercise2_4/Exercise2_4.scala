package fpinscala.exercise2_4

import fpinscala.exercise2_3.curry
import org.scalatest.FunSuite

class Exercise2_4 extends FunSuite {

  test("uncurry") {
    assert(uncurry(curry((a: String, b: String) => a + b))("Hello, ", "World") == "Hello, World")
    assert(uncurry(curry(uncurry(curry((a: String, b: String) => a + b))))("Hello, ", "World") == "Hello, World")
  }

}
