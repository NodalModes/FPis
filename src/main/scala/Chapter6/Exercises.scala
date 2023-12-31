//package Chapter6
//
//import Chapter6.Exercises.SimpleRNG.nonNegativeInt
//
//import scala.::
//import scala.annotation.tailrec
//
//object Exercises {
//
//  trait RNG {
//    def nextInt: (Int, RNG)
//  }
//
//  type Rand[+A] = RNG => (A, RNG)
//
//  val int: Rand[Int] = _.nextInt
//
//  def unit[A](a: A): Rand[A] =
//    rng => (a, rng)
//
//  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
//    rng => {
//      val (a, rng2) = s(rng)
//      (f(a), rng2)
//    }
//
//  def nonNegativeEven: Rand[Int] =
//    map(nonNegativeInt)(i => i - 1 % 2)
//
//  // Exercise 6.5
//  /*
//  def double(rng: RNG): (Double, RNG) = {
//    val (i, rng2) = nonNegativeInt(rng)
//    (i / (Int.MaxValue.toDouble + 1), rng2)
//  }
//   */
//  val double: Rand[Double] =
//    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))
//
//  // Exercise 6.6
//  def map2[A, B, C](ra: Rand[A], rb:Rand[B])(f: (A, B) => C): Rand[C] = {
//    rng => {
//      val (a, r1) = ra(rng)
//      val (b, r2) = rb(r1)
//      (f(a, b), r2)
//    }
//  }
//
//  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
//    map2(ra, rb)((_, _))
//
//  val randIntDouble: Rand[(Int, Double)] =
//    both(int, double)
//
//  val randDoubleInt: Rand[(Double, Int)] =
//    both(double, int)
//
//  // Exercise 6.7
//
//  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
//    fs.foldRight(unit(List[A]()))((ra, rla) => map2(ra, rla)(_ :: _))
//  }
//
//  def ints(count: Int): Rand[List[Int]] =
//    sequence(List.fill(count)(int))
//
//  // Exercise 6.8
//
//  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
//    rng => {
//      val (v, r) = f(rng)
//      g(v)(r)
//    }
//  }
//
//  def nonNegativeLessThan(n: Int): Rand[Int] = {
//    flatMap(nonNegativeInt) {
//      i => {
//        val mod = i % n
//        if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
//      }
//    }
//  }
//
//  // Exercise 6.9
//
//  /*
//  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
//    rng => {
//      val (a, rng2) = s(rng)
//      (f(a), rng2)
//    }
//   */
//  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
//    flatMap(s)(a => unit(f(a)))
//
//  /*
//  def map2[A, B, C](ra: Rand[A], rb:Rand[B])(f: (A, B) => C): Rand[C] = {
//    rng => {
//      val (a, r1) = ra(rng)
//      val (b, r2) = rb(r1)
//      (f(a, b), r2)
//    }
//  }
//   */
//  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb:Rand[B])(f: (A, B) => C): Rand[C] = {
//
//  }
//
//  case class SimpleRNG(seed: Long) extends RNG {
//    def nextInt: (Int, RNG) = {
//      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
//      val nextRNG = SimpleRNG(newSeed)
//      val n = (newSeed >>> 16).toInt
//      (n, nextRNG)
//    }
//  }
//
//  object SimpleRNG {
//
//    def randomPair(rng: RNG): ((Int, Int), RNG) = {
//      val (i1, rng2) = rng.nextInt
//      val (i2, rng3) = rng2.nextInt
//      ((i1, i2), rng3)
//    }
//
//    // Exercise 6.1
//    def nonNegativeInt(rng: RNG): (Int, RNG) = {
//      val (i, r) = rng.nextInt
//      (if (i < 0) -(i + 1) else i, r)
//    }
//
//    // Exercise 6.2
//    def double(rng: RNG): (Double, RNG) = {
//      val (i, rng2) = nonNegativeInt(rng)
//      (i / (Int.MaxValue.toDouble + 1), rng2)
//    }
//
//    // Exercise 6.3
//    def intDouble(rng: RNG): ((Int, Double), RNG) = {
//      val (i, r1) = rng.nextInt
//      val (d, r2) = double(r1)
//      ((i, d), r2)
//    }
//
//    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
//      val ((i, d), r) = intDouble(rng)
//      ((d, i), r)
//    }
//
//    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
//      val (d1, r1) = double(rng)
//      val (d2, r2) = double(r1)
//      val (d3, r3) = double(r2)
//      ((d1, d2, d3), r3)
//    }
//
//    // Exercise 6.4
//    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
//      if (count <= 0) (List(), rng) else {
//        val (i, r1) = rng.nextInt
//        val (is, r2) = ints(count - 1)(r1)
//        (i :: is, r2)
//      }
//    }
//
//    def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
//      @tailrec
//      def go(count: Int, r: RNG, is: List[Int]): (List[Int], RNG) =
//        if (count <= 0)
//          (is, r)
//        else {
//          val (i, r1) = r.nextInt
//          go(count - 1, r1, i :: is)
//        }
//
//      go(count, rng, List())
//    }
//  }
//
//}
