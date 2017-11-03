package fpinscala

import scala.annotation.tailrec

package object exercise2_2 {

  def isSorted[A](as:Array[A], ordered: (A,A) => Boolean): Boolean = {
    //ordered(0, 1) && ordered(1, 2) && ...
    @tailrec
    def loop(n:Int):Boolean = n match {
      case _ if n+1 == as.size-1 => ordered(as(n), as(n+1))
      case _ => ordered(as(n), as(n+1)) && loop(n+1)
    }
    loop(0)
  }

}
