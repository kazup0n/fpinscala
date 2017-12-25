package fpinscala.exercise4

import fpinscala.exercise4.Option.of
import org.scalatest.FunSuite

class OptionTest extends FunSuite {


  test("exercise 4.1 map") {
    assert(Some("123").map(_.toInt) === Some(123))
    assert(of[String](null).map(_.toInt) === None)
  }

  test("exercise 4.1 flatMap") {
    assert(of("123").flatMap(of(_)) === Some("123"))
    assert(of("123").flatMap(_ => None) === None)

    assert(of[String](null).flatMap(n => Some(n.toInt)) === None)
    assert(of[String](null).flatMap(_ => None) === None)
  }

  test("exercise 4.1 getOrElse") {
    assert(of("123").getOrElse("456") === "123")
    assert(of(null).getOrElse("456") === "456")
  }

  test("exercise 4.1 orElse") {
    assert(of("123").orElse(Some("456")) === Some("123"))
    assert(of(null).orElse(Some("456")) === Some("456"))
  }

  test("exercise 4.1 filter") {
    assert(of(2).filter(_ % 2 == 0) === Some(2))
    assert(of(1).filter(_ % 2 == 0) === None)

    val none: Option[Int] = None
    assert(none.filter(_ % 2 == 0) === None)
  }

  test("exercise 4.2 mean") {
    assert(mean(Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) == Some(5.5))
  }

  test("exercise 4.2 variance") {
    assert(variance(Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) == Some(8.25))
  }

  test("exercise 4.3 map2") {
    assert(
      Option.map2(of(123), of("2")) { (a, b) =>
        a * b.toInt
      }
        ===
        Some(246)
    )
  }

  test("exercise 4.4 sequence") {
    assert(
      Option.sequence(List(of("Hello"), of(", "), of("World"), of("!"))).map(ss => ss.mkString(""))
        ===
        Some("Hello, World!")
    )

    assert(
      Option.sequence(List(of("Hello"), None, of("World"))).map(_.mkString(""))
        ===
        None
    )
  }

  test("exercise 4.5 traverse") {
    assert(
      Option.traverse(List("1", "2", "3", "4"))(n => Option.Try(n.toInt))
        ===
        Some(List(1, 2, 3, 4))
    )

    assert(
      Option.traverse(List("1", "2", "three", "4"))(n => Option.Try(n.toInt))
        ===
        None)
  }

}
