package Chapter2

import Chapter2.Ex2p1.fib
import org.scalatest.funsuite.AnyFunSuiteLike

class Ex2p1Test extends AnyFunSuiteLike {

  test("testFib") {
    assert(fib(-1) === 0)
    assert(fib(0) === 0)
    assert(fib(1) === 1)
    assert(fib(2) === 1)
    assert(fib(3) === 2)
    assert(fib(4) === 3)
    assert(fib(5) === 5)
    assert(fib(6) === 8)
    assert(fib(7) === 13)
    assert(fib(8) === 21)
    assert(fib(9) === 34)
  }

}
