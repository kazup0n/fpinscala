package fpinscala.exercise4

trait Partial[+E,+A] {

  def map[B](f: A => B): Partial[E, B] = this match {
    case Errors(e) => Errors(e)
    case Success(v) => Success(f(v))
  }

  def flatMap[EE >: E, B](f: A=>B): Partial[EE, B] = this match {
    case Success(v) => Success(f(v))
    case Errors(es) => Errors(es)
  }

  def map2[EE >: E, B, C](b: Partial[EE, B])(f:(A, B)=>C) = (this, b) match {
    case (Success(aa), Success(bb)) => Success(f(aa, bb))
    case (Errors(e1), Errors(e2)) => Errors(e1++e2)
    case (Errors(e), _) => Errors(e)
    case (_, Errors(e)) => Errors(e)
  }

}
case class Errors[+E](get: Seq[E]) extends Partial[E,Nothing]
case class Success[+A](get: A) extends Partial[Nothing,A]

object Partial {


  def traverse[E, A, B](es:List[A])(f: A=>Partial[E, B]): Partial[E, List[B]] =
    es.foldRight[Partial[E, List[B]]](Success(Nil)) { (a, b) => f(a).map2(b)(_::_)}



}

