package Chapter2

import org.scalatest.funsuite.AnyFunSuiteLike

class Ex2p2Test extends AnyFunSuiteLike {

  def inOrder(x: Int, y: Int): Boolean = x <= y

  assert(Ex2p2.isSorted(Array(1, 2, 3), inOrder) === true)
  assert(Ex2p2.isSorted(Array(1, 2), inOrder) === true)
  assert(Ex2p2.isSorted(Array(1), inOrder) === true)
  assert(Ex2p2.isSorted(Array(3, 2, 1), inOrder) === false)
  assert(Ex2p2.isSorted(Array(2, 2, 2), inOrder) === true)

}
