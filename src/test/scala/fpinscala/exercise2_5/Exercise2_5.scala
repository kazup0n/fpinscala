package fpinscala.exercise2_5

import org.scalatest.FunSuite

class Exercise2_5 extends FunSuite {


  test("compose") {

    assert(
      compose(
        (str: String) => "** " + str + " **",
        (name: String) => "Hello, " + name)("World")
        === "** Hello, World **")

  }

}
