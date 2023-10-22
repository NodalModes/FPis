package Chapter2

import scala.annotation.tailrec

object Ex2p1 {

  def fib(n: Int): Int = {

    @tailrec
    def go(previous: Int, current: Int, goalIndex: Int): Int = {
      if (goalIndex < 1) previous
      else go(current, previous + current, goalIndex - 1)
    }

    go(0, 1, n)
  }
}
