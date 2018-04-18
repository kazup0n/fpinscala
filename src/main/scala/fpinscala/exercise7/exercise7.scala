package fpinscala

package object exercise7 {

  def sum(ints:IndexedSeq[Int]):Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else ints.splitAt(ints.length /2) match {
      case (l,r) => sum(l) + sum(r)
    }
}

package exercise7
