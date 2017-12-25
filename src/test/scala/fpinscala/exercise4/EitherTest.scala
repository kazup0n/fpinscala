package fpinscala.exercise4

import fpinscala.exercise4.Either.{failure, successful}
import org.scalatest.FunSuite

class EitherTest extends FunSuite {

  test("Exercise 4.6 map") {
    assert(successful[String, String]("123").map(_.toInt) === Right(123))
    assert(failure[String, String]("NaN").map(_.toInt) === Left("NaN"))
  }

  test("Exercise 4.6 flatMap") {
    //レシーバがRight
    assert(
      successful[String, String]("123").flatMap(n => successful(n.toInt))
        ===
        Right(123))

    assert(
      successful[String, String]("123").flatMap(n => failure("NaN"))
        ===
        Left("NaN"))

    //レシーバがLeft
    assert(failure("NaN").flatMap(_ => successful(123)) === Left("NaN"))
    assert(failure("NaN").flatMap(_ => failure(123)) === Left("NaN"))
  }

  test("exercise 4.6 orElse") {
    //レシーバがRight
    assert(
      successful[String, String]("123").orElse[String, String](successful[String, String]("123"))
        ==
        Right("123"))
    //レシーバがLeft
    assert(
      failure[String, String]("123").orElse[String, String](successful[String, String]("456"))
        == Right("456")
    )
    assert(
      failure[String, String]("123").orElse[String, String](failure[String, String]("456"))
        == Left("456")
    )
  }

  test("exercise 4.7 sequence") {
    val success = List(successful("123"), successful("456"), successful("789"))
    val fail = List(successful("123"), failure("fail"), failure("fail2"))

    assert(Either.sequence(success) === successful(List("123", "456", "789")))
    assert(Either.sequence(fail) === failure("fail"))
  }

  test("exercise 4.7 traverse") {
    assert(
      Either.traverse(List("1", "2", "3", "4"))(n => Either.Try(n.toInt))
        ===
        successful(List(1, 2, 3, 4))
    )

    Either.traverse(List("1", "2", "ABC", "4"))(n => Either.Try(n.toInt)) match {
      case Left(e) => assert(e.getMessage == "For input string: \"ABC\"")
    }
  }


  test("exercise 4.8") {

    case class Person(name: Name, age: Age)
    sealed class Name(val value: String)
    sealed class Age(val value: Int)

    def mkName(name: String): Either[String, Name] =
      if (name == null || name.isEmpty) Left("Name is empty.")
      else Right(new Name(name))

    def mkAge(age: Int): Either[String, Age] =
      if (age < 0) Left("Age is out of range.")
      else Right(new Age(age))

    def mkPerson(name: String, age: Int): Either[String, Person] =
      mkName(name).map2(mkAge(age))(Person(_, _))

    def mkPerson2(name: String, age: Int): Either[Seq[String], Person] =
      compose(mkName(name), mkAge(age))(Person(_, _))

    def compose[E, A, B, C](a: => Either[E, A], b: => Either[E, B])(f: (A, B) => C): Either[Seq[E], C] =
      (a, b) match {
        case (Right(aa), Right(bb)) => Right(f(aa, bb))
        case (Left(e1), Left(e2)) => Left(Seq(e1, e2))
        case (Left(e), _) => Left(Seq(e))
        case (_, Left(e)) => Left(Seq(e))
      }

    mkPerson2("", -1) match {
      case Left(Seq(e1, e2)) => {
        assert(e1 == "Name is empty.")
        assert(e2 == "Age is out of range.")
      }
    }
  }

  test("exercise 4.8 with Partial"){

    case class Person(name: Name, age: Age)
    sealed class Name(val value: String)
    sealed class Age(val value: Int)


    def mkName(name: String): Partial[String, Name] =
      if (name == null || name.isEmpty) Errors(Seq("Name is empty."))
      else Success(new Name(name))

    def mkAge(age: Int): Partial[String, Age] =
      if (age < 0) Errors(Seq("Age is out of range."))
      else Success(new Age(age))

    def mkPerson(name: String, age: Int): Partial[String, Person] =
      mkName(name).map2(mkAge(age))(Person(_, _))

    mkPerson("", -1) match {
      case Errors(es) => assert(es == Seq("Name is empty.", "Age is out of range."))
    }


  }

}
