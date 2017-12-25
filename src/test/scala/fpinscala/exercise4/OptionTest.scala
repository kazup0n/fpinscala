package fpinscala.exercise4

import fpinscala.exercise4.Option.of
import org.scalatest.FunSuite

class OptionTest extends FunSuite {


  test("exercise 4.1 map"){
    assert(Some("123").map(_.toInt) === Some(123))
    assert(of[String](null).map(_.toInt) === None)
  }

  test("exercise 4.1 flatMap"){
    assert(of("123").flatMap(of(_)) === Some("123"))
    assert(of("123").flatMap(_=>None) === None)

    assert(of[String](null).flatMap(n=>Some(n.toInt)) === None)
    assert(of[String](null).flatMap(_=>None) === None)
  }

  test("exercise 4.1 getOrElse"){
    assert(of("123").getOrElse("456") === "123")
    assert(of(null).getOrElse("456") === "456")
  }

  test("exercise 4.1 orElse"){
    assert(of("123").orElse(Some("456")) === Some("123"))
    assert(of(null).orElse(Some("456")) === Some("456"))
  }

  test("exercise 4.1 filter"){
    assert(of(2).filter(_%2==0) === Some(2))
    assert(of(1).filter(_%2==0) === None)

    val none:Option[Int] = None
    assert(none.filter(_%2==0) === None)
  }

}
