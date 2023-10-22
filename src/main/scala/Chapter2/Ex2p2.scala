package Chapter2

import scala.annotation.tailrec

object Ex2p2 {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def go(i: Int, previous: A): Boolean = {
      if (i >= as.length) true
      else {
        val current = as(i)
        if (!ordered(previous, current)) false
        else go(i + 1, current)
      }
    }

    go(1, as(0))
  }

}
