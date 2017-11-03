package fpinscala

package object exercise2_5 {

  def compose[A, B, C](f: B => C, g: A => B): A => C = g.andThen(f)

}
