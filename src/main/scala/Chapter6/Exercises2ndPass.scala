package Chapter6

import scala.annotation.tailrec

object Exercises2ndPass {

}

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = rng2.nextInt
    ((i1, i2), rng3)
  }

  // Exercise 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  } // Exercise 6.1

  // Exercise 6.2
  def _double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  } // Exercise 6.2

  // Exercise 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = _double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = _double(rng)
    val (d2, r2) = _double(r1)
    val (d3, r3) = _double(r2)
    ((d1, d2, d3), r3)
  } // Exercise 6.3

  // Exercise 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(count: Int, r: RNG, l: List[Int]): (List[Int], RNG) = {
      if (count > 0) {
        val (i, r1) = r.nextInt
        go(count - 1, r1, i :: l)
      } else (l, r)
    }

    go(count, rng, List())
  } // Exercise 6.4

  // type alias for the RNG state action data type
  type Rand[A] = RNG => (A, RNG)
  //  type Rand[A] = State[RNG, A]

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = (a, _)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  // Exercise 6.5
  val double: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1)) // Exercise 6.5

  // Exercise 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    } // Exercise 6.6

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  // Exercise 6.7
  def sequence[A](ras: List[Rand[A]]): Rand[List[A]] = {
    ras.foldRight(unit(List[A]()))((ra, la) => map2(ra, la)(_ :: _))
  }

  def intsViaSequence(count: Int)(rng: RNG): Rand[List[Int]] =
    sequence(List.fill(count)(int)) // Exercise 6.7

  // Exercise 6.8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (v, r) = f(rng)
      g(v)(r)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    } // Exercise 6.8

  // Exercise 6.9
  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b))) // Exercise 6.9

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)
}

//type State[S, +A] = S => (A, S)
case class State[S, +A](run: S => (A, S)) {

  // Exercise 6.10
  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State(s => {
      val (v, s1) = run(s)
      f(v).run(s1)
    })
  }

  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b))) // Exercise 6.10

}

object State {
  // Exercise 6.10
  def unit[S, A](a: A): State[S, A] = State((a, _))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List[A]()))((sa, la) => sa.map2(la)(_ :: _)) // Exercise 6.10

  def get[S]: State[S, S] = State(s => (s, s))
  
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}