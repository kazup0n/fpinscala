package fpinscala.exercise4

sealed trait Option[+A] {

  def map[B](f: A=>B):Option[B] = this match {
    case Some(v) => Some(f(v))
    case None => None
  }
  def flatMap[B](f: A=>Option[B]):Option[B] = map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(v) => v
    case None => default
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = flatMap { v=>
    if(f(v)) Some(v)
    else None
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def of[A](v: A):Option[A] = {
    if(v == null) None
    else Some(v)
  }

}
