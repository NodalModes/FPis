package Chapter6
// Exercise 6.11

import Chapter6.State.{get, modify, sequence}
import Chapter6.go.simulateMachine

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

// inserting a coin into a locked machine will cause it to unlock if there's any candy left
// turning the knob on an unlocked machine will cause it to dispense candy and become locked
// turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing
// a machine that's out of candy ignores all inputs

/*
sequence(inputs          map (i         => modify[Machine]          i         compose update                         ))
        |-List[State[Machine, (Int,Int)]]-----------------------------------------------------------------------------|
         |-List[Input]-|     |-Input => State[Machine, (Int, Int)]---------------------------------------------------|
                              |-Input-|                             |-Input-|
                                           |-State[Machine, Unit]-| |-Machine => Machine----------------------------|
                                                                                      |-Input => Machine => Machine-|

 */

//                                                       (coins, candies)

object go {
  def mFunc(i: Input): State[Machine, Unit] = {
    modify[Machine](update(i))
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    for {
      //  _ <- sequence(inputs map (modify[Machine] _ compose update))
      _ <- sequence(inputs map(i => modify[Machine](update(i))))
//      _ <- sequence(inputs map (i => mFunc(i)))
      s <- get
    } yield (s.coins, s.candies)
  }


def update: Input => Machine => Machine =
  (i: Input) => (m: Machine) =>
    (i, m) match {
      case (_, Machine(_, c, _)) if c <= 0 => m
      case (Coin, Machine(false, _, _)) => m
      case (Coin, Machine(true, candy, coin)) => Machine(locked = false, candy, coin + 1)
      case (Turn, Machine(true, _, _)) => m
      case (Turn, Machine(false, candy, coin)) => Machine(locked = true, candy - 1, coin)
    }

def main(args: Array[String]): Unit = {

  val machine = Machine(locked = true, candies = 5, coins = 10)
  val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
  val ((coins, candies), m) = simulateMachine(inputs).run(machine)
  println(s"coins: $coins candies: $candies")
}

}

