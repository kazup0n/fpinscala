package fpinscala

import fpinscala.exercise7.Par.Par

package object exercise7 {

  def _sum(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else ints.splitAt(ints.length / 2) match {
      case (l, r) => _sum(l) + _sum(r)
    }


  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else ints.splitAt(ints.length / 2) match {
      case (l, r) => Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }
}
