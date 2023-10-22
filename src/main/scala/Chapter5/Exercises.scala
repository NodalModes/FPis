package Chapter5

import Chapter3.ListExercises
import Chapter5.Exercises.Empty.zipWith
import Chapter5.Exercises.Stream.{cons, empty, unfold}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Exercises {

  sealed trait Stream[+A] {

    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, _) => Some(h())
    }

    // Exercise 5.1
    def toListDangerous: List[A] = this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toListDangerous
    }

    def toList: List[A] = {
      @tailrec
      def go(stream: Stream[A], acc: List[A]): List[A] = stream match {
        case Empty => acc.reverse
        case Cons(h, t) => go(t(), h() :: acc)
      }

      go(this, Nil)
    }

    def toListFast: List[A] = {
      val buffer = new ListBuffer[A]

      def go(stream: Stream[A]): List[A] = stream match {
        case Cons(h, t) => {
          buffer += h()
          go(t())
        }
        case Empty => buffer.toList
      }

      go(this)
    }

    // Exercise 5.2
    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case _ => empty
    }

    @tailrec
    final def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }

    // Exercise 5.3
    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
      case _ => empty
    }

    def exists(p: A => Boolean): Boolean = this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    // Exercise 5.4
    def forAllMine(p: A => Boolean): Boolean = this match {
      case Cons(h, t) => p(h()) && t().forAll(p)
      case _ => true
    }

    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((a, b) => p(a) && b)

    // Exercise 5.5
    def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
      foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

    // Exercise 5.6
    def headOptionViaFoldRight: Option[A] =
      foldRight(None: Option[A])((a, _) => Some(a))

    // Exercise 5.7
    def map[B](f: A => B): Stream[B] =
      foldRight(empty: Stream[B])((a, b) => cons(f(a), b))

    def filter(p: A => Boolean): Stream[A] =
      foldRight(empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else b)

    def append[B >: A](s: => Stream[B]): Stream[B] =
      foldRight(s)((a, b) => cons(a, b))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(empty: Stream[B])((a, b) => f(a) append b)


    def find(p: A => Boolean): Option[A] =
      filter(p).headOption


    // Exercise 5.13
    def mapViaUnfold[B](f: A => B): Stream[B] =
      unfold[B, Stream[A]](this) {
        case Empty => None
        case Cons(h, t) => Some((f(h()), t()))
      }

    def takeViaUnfold(n: Int): Stream[A] =
      unfold((n, this)) {
        case (n, Cons(h, t)) if n > 1 => Some((h(), (n - 1, t())))
        case (1, Cons(h, t)) => Some(h(), (0, empty))
        case _ => None
      }

    def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
      unfold(this) {
        case Cons(h, t) if p(h()) => Some(h(), t())
        case _ => None
      }

    def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
      unfold((this, s2)) {
        case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
        case _ => None
      }

    def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
      zipWithAll(s2)((_, _))

    def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
      Stream.unfold((this, s2)) {
        case (Empty, Empty) => None
        case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
        case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
        case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
      }

    // Exercise 5.14
    // this is bad for a variety of reasons, im sure
    def startsWithMine[A](s: Stream[A]): Boolean =
      zipWith[A, Boolean](s)((a, b) => a == b).forAll(v => v)

    def startsWith[A](s: Stream[A]): Boolean = {
      zipAll(s).takeWhile(!_._2.isEmpty) forAll { case (h, h2) => h == h2 }
      //                  tuple => !tuple._2.isEmpty
    }

    // Exercise 5.15
    def tailsMine: Stream[Stream[A]] =
      unfold(this) {
        case Cons(h, t) => Some(cons(h(), t()), t())
        case _ => None
      } append Stream(empty)

    def tails: Stream[Stream[A]] =
      unfold(this) {
        case Empty => None
        case s => Some((s, s drop 1))
      } append Stream(empty)

    def hasSubsequence[A](s: Stream[A]): Boolean =
      tails exists (_ startsWith s)

    // Exercise 5.16
    // foldRight[B](z: => B)(f: (A, => B) => B): B
    import Chapter3.ListExercises._
    // Actually this was supposed to return a Stream my bad. It works tho
    def scanRightMine[B](z: => B)(f: (A, => B) => B): List[B] =
      foldRight[List[B]](List(z))((a, lb) => lb match {
        case ListExercises.Cons(lbHead, _) => ListExercises.Cons(f(a, lbHead), lb)
        case ListExercises.Nil => Nil
      })

    def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
      foldRight((z, Stream(z)))((a, p0) => {
        // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
        lazy val p1 = p0
        val b2 = f(a, p1._1)
        (b2, cons(b2, p1._2))
      })._2

  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))


    // Exercise 5.8
    def constant[A](a: A): Stream[A] = {
      lazy val tail: Stream[A] = Cons(() => a, () => tail)
      tail
    }

    // Exercise 5.9
    def from(n: Int): Stream[Int] =
      cons(n, from(n + 1))

    // Exercise 5.10
    def fibs: Stream[Int] = {
      def go(a: Int, b: Int): Stream[Int] =
        cons(a, go(b, a + b))

      go(0, 1)
    }

    // Exercise 5.11
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }

    // exercise 5.12
    def onesViaUnfold: Stream[Int] =
      unfold[Int, Int](1)(_ => Some((1, 1)))

    def constantViaUnfold[A](a: A): Stream[A] =
      unfold[A, A](a)(_ => Some(a, a))

    def fromViaUnfold(n: Int): Stream[Int] =
      unfold[Int, Int](n)(s => Some(s, s + 1))

    def fibsViaUnfold: Stream[Int] =
      unfold[Int, (Int, Int)]((0, 1)) { case (a, b) => Some(b, (b, a + b)) }


  }

}
