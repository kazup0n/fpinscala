package fpinscala.exercise3

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(left: Tree[A], right: Tree[A]) => Branch(map(left)(f), map(right)(f))
  }


  def depth[A](t: Tree[A], search: Leaf[A]): Int = t match {
    case l: Leaf[A] => if (l == search) 1 else 0
    case Branch(left, right) => {
      val n = depth(left, search) max depth(right, search) max 0
      if (n > 0) n + 1 else 0
    }
  }

  def depth[A](t: Tree[A]):Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => (depth(l) max depth(r)) + 1
  }



  def max(t: Tree[Int]): Int = t match {
    case Leaf(x: Int) => x
    case Branch(left, right) => max(left) max max(right)
  }


  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def fold[A, B](t: Tree[A])(f: (A)=> B)(g: (B, B)=>B):B = t match {
    case Leaf(a) => f(a)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

}