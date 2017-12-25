package fpinscala.exercise4

sealed trait Option[+A] {

  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = flatMap { v =>
    if (f(v)) Some(v)
    else None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def map[B](f: A => B): Option[B] = this match {
    case Some(v) => Some(f(v))
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(v) => v
    case None => default
  }
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {


  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (_a => b map (_b => f(_a, _b)))

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {
      case _: Exception => None
    }

  def sequence[A](seq: List[Option[A]]): Option[List[A]] = {
    seq.foldLeft(of(List[A]())) { (z, s) =>
      z.flatMap { zz =>
        s map { ss =>
          zz :+ ss
        }
      }
    }
  }

  def of[A](v: A): Option[A] = {
    if (v == null) None
    else Some(v)
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldLeft(of(List[B]())) { (z, aa) =>
      z flatMap { zz => //zz:List[B]
        // f(aa):Option[B]
        f(aa).map(zz :+ _)
      }
    }


}

