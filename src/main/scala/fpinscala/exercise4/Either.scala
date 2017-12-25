package fpinscala.exercise4

sealed trait Either[+E, +A] {
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => Right(v)
    case _ => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = flatMap(aa => b.map(f(aa, _)))

  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(v) => Right(f(v))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => f(v)
    case Left(e) => Left(e)
  }

}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def failure[E, A](e: E): Either[E, A] = Left(e)

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es.foldLeft(successful[E, List[A]](List()))((z, e) => z flatMap (zz => e map (zz :+ _)))

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldLeft(successful[E, List[B]](List()))((z, a) => z flatMap (zz => f(a) map (b => zz :+ b)))

  def successful[E, A](a: A): Either[E, A] = Right(a)


}