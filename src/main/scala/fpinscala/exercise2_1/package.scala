package fpinscala


package object exercise2_1 {

  def fib(n: Int): Int =
    n match {
      case _ if n <= 0 => throw new IllegalArgumentException("n should be over 0")
      case 1 => 0
      case 2 => 1
      case _ if n > 2 => fib(n - 1) + fib(n - 2)
    }

}
