package fpinscala.exercise6


case class State[S, +A](run: S => (A, S)) {


  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => run(s) match {
    case (a, next) => f(a).run(next)
  })

  def map[B](f: A => B): State[S, B] = flatMap { a => State.unit(f(a)) }

  def map2[B, C](sb: State[S, B])(f: (A,B) => C):State[S, C] = for {
    a <- this
    b <- sb
  } yield f(a, b)


  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

}

object State {


  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = fs.foldRight(State.unit(List.empty): State[S, List[A]])((s, acc) => s.map2(acc)(_ +: _))




}

object RandState {

  type Rand[A] = State[RNG, A]

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = s map f

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = f flatMap g

  def unit[A](a: A): Rand[A] = State.unit(a)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = State.sequence(fs)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ra.map2(rb)(f)


  def int:Rand[Int] = State(_.nextInt)

  def nonNegativeInt:Rand[Int] = int.map(Math.abs(_))

}


