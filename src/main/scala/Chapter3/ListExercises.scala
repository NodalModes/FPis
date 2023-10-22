package Chapter3

import scala.annotation.tailrec

object ListExercises {
  // for 3.1 the answer is just 3

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  // companion object
  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(head, tail) => head + sum(tail)
    }

    def product(doubles: List[Double]): Double = doubles match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(head, tail) => head * product(tail)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    // Exercise 3.2
    def tail[A](list: List[A]): List[A] = list match {
      case Nil => Nil // for this case they choose this instead: sys.error("tail of empty list")
      case Cons(_, tail) => tail
    }

    // Exercise 3.3
    def setHead[A](list: List[A], newHead: A): List[A] = list match {
      case Nil => List(newHead) // they choose instead: sys.error("setHead on empty list")
      case Cons(_, tail) => Cons(newHead, tail)
    }

    // Exercise 3.4
    @tailrec
    def drop[A](list: List[A], n: Int): List[A] = {
      if (n <= 0) list
      else list match {
        case Nil => sys.error("drop on empty List") // they chose to return Nil here because otherwise you would have to know the length of the list before you used this method
        case Cons(_, tail) => drop(tail, n - 1)
      }
    }

    // Exercise 3.5
    @tailrec
    def dropWhile[A](list: List[A], predicate: A => Boolean): List[A] = list match {
      case Cons(head, tail) if predicate(head) => dropWhile(tail, predicate) // this uses the pattern guard method
      case _ => list
    }

    // Exercise 3.6
    def init[A](list: List[A]): List[A] = list match {
      case Nil => sys.error("init on empty list")
      case Cons(_, Nil) => Nil
      case Cons(head, tail) => Cons(head, init(tail))
    }

    // Exercise 3.7
    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    val list = List(1, 2, 3, 0, 4, 5, 6)

    def prod(x: Int, y: Int): Int = (x, y) match {
      case (0, _) => 0
      case (_, 0) => 0
      case (xv, yv) => xv * yv
    }

    val zeroShortCircuit = foldRight(list, 1)(prod)
    /*
    foldRight(Cons(1, Cons(2, Cons(3, Cons(0, Cons(4, Cons(5, Cons(6, Nil))))))), 1)(prod)
    prod(1, foldRight(Cons(2, Cons(3, Cons(0, Cons(4, Cons(5, Cons(6, Nil)))))), 1)(prod))
    prod(1, prod(2, foldRight(Cons(3, Cons(0, Cons(4, Cons(5, Cons(6, Nil))))), 1)(prod)))
    prod(1, prod(2, prod(3, foldRight(Cons(0, Cons(4, Cons(5, Cons(6, Nil)))), 1)(prod))))
    prod(1, prod(2, prod(3, prod(0, foldRight(Cons(4, Cons(5, Cons(6, Nil))), 1)(prod)))))
    prod(1, prod(2, prod(3, prod(0, prod(4, foldRight(Cons(5, Cons(6, Nil)), 1)(prod))))))
    prod(1, prod(2, prod(3, prod(0, prod(4, prod(5, foldRight(Cons(6, Nil), 1)(prod)))))))
    prod(1, prod(2, prod(3, prod(0, prod(4, prod(5, prod(6, foldRight(Nil, 1)(prod))))))))
    prod(1, prod(2, prod(3, prod(0, prod(4, prod(5, prod(6, 1)))))))
    prod(1, prod(2, prod(3, prod(0, prod(4, prod(5, 6))))))
    prod(1, prod(2, prod(3, prod(0, prod(4, 30)))))
    prod(1, prod(2, prod(3, prod(0, 120))))
    prod(1, prod(2, prod(3, 0)))
    prod(1, prod(2, 0))
    prod(1, 0)
    0
    It looks like short-circuiting is not possible even with a short-circuit implemented in the function passed to foldRight.
    */

    // Exercise 3.8
    /*We should get back the original list*/

    // Exercise 3.9
    def length[A](as: List[A]): Int = {
      foldRight(as, 0)((_, y) => 1 + y)
    }

    // Exercise 3.10
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
    }

    // Exercise 3.11
    def sumLeft(list: List[Int]): Int = {
      foldLeft(list, 0)(_ + _)
    }

    def productLeft(list: List[Int]): Int = {
      foldLeft(list, 1)(_ * _)
    }

    def lengthLeft[A](list: List[A]): Int = {
      foldLeft(list, 0)((x, _) => 1 + x)
    }

    // Exercise 3.12
    def reverse[A](list: List[A]): List[A] = {
      foldLeft(list, Nil: List[A])((acc, e) => Cons(e, acc))
    }

    // Exercise 3.13
    def foldRightInTermsOfLeft[A, B](list: List[A], z: B)(f: (A, B) => B): B = {
      val reverse = List.reverse(list)
      foldLeft(reverse, z)((a, b) => f(b, a))
    }

    /* HOLY SHIT THIS IS INSANE. THE ANSWER ON THE SITE DOESN'T EVEN COMPILE!? */
    //    def foldLeftViaFoldRight[A,B](l: List[A], z: B, f: (B,A) => B): B =
    //      foldRight[A, B => B](l, (b:B) => b, (a: A, g : B) => (b => g(f(b,a))))asdflkjasdhflkasdfjngfkl;dfs(z)


    // Exercise 3.14
    def append[A](list1: List[A], list2: List[A]): List[A] = {
      foldRight(list1, list2)(Cons(_, _))
    }

    // Exercise 3.15
    def concat[A](listList: List[List[A]]): List[A] = {
      foldRight(listList, Nil: List[A])(append)
    }

    // Exercise 3.16
    def vectorStyleIncrement(list: List[Int]): List[Int] = {
      foldRight(list, Nil: List[Int])((a, b) => Cons(a + 1, b))
    }

    // Exercise 3.17
    def doubleToString(list: List[Double]): List[String] = {
      foldRight(list, Nil: List[String])((a, b) => Cons(a.toString, b))
    }

    // Exercise 3.18
    def map[A, B](list: List[A])(f: A => B): List[B] = {
      foldRight(list, Nil: List[B])((a, b) => Cons(f(a), b))
    }

    // Exercise 3.19
    def filter[A](list: List[A])(f: A => Boolean): List[A] = {
      foldRight(list, Nil: List[A])((a, b) => if (f(a)) Cons(a, b) else b)
    }

    // Exercise 3.20
    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = {
      foldRight(list, Nil: List[B])((a, b) => append(f(a), b))
    }

    // or
    def theirFlatMap[A, B](list: List[A])(f: A => List[B]): List[B] =
      concat(map(list)(f))

    // Exercise 3.21
    def filterViaFlatMap[A](list: List[A])(f: A => Boolean): List[A] = {
      flatMap[A, A](list)(a => if (f(a)) List(a) else Nil)
    }

    // Exercise 3.22
    def addElementWise(list1: List[Int], list2: List[Int]): List[Int] = (list1, list2) match {
      case (Nil, Nil) => Nil
      case (Nil, l) => l
      case (l, Nil) => l
      case (Cons(head1, tail1), Cons(head2, tail2)) => Cons(head1 + head2, addElementWise(tail1, tail2))
    }

    // Exercise 2.23
    def zipWith[A, B, C](list1: List[A], list2: List[B], z1: A, z2: B)(f: (A, B) => C): List[C] = (list1, list2) match {
      case (Nil, Nil) => Nil
      case (Nil, Cons(h, t)) => Cons(f(z1, h), zipWith(Nil, t, z1, z2)(f))
      case (Cons(h, t), Nil) => Cons(f(h, z2), zipWith(t, Nil, z1, z2)(f))
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2, z1, z2)(f))
    }

    // or theirs
    def tailRecZipWith[A, B, C](listA: List[A], listB: List[B])(f: (A, B) => C): List[C] = {
      @tailrec
      def loop(a: List[A], b: List[B], result: List[C]): List[C] = (a, b) match {
        case (Nil, _) => result
        case (_, Nil) => result
        case (Cons(h1, t1), Cons(h2, t2)) => loop(t1, t2, Cons(f(h1, h2), result))
      }

      reverse(loop(listA, listB, Nil))
    }

    // Exercise 3.24
    @tailrec
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
      (sup, sub) match {
        case (Cons(_, _), Cons(_, _)) if List.length(sup) < List.length(sub) => false
        case (Nil, Nil) => true
        case (Nil, _) => false
        case (_, Nil) => true
        case (Cons(supHead, supTail), Cons(subHead, subTail)) =>
          if (supHead == subHead) hasSubsequence(supTail, subTail)
          else hasSubsequence(List.dropWhile[A](sup, _ != subHead), sub)
      }
    }

    // or theirs
    /*
    @annotation.tailrec
    def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
      case (_,Nil) => true
      case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
      case _ => false
    }
    @annotation.tailrec
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
      case Nil => sub == Nil
      case _ if startsWith(sup, sub) => true
      case Cons(_,t) => hasSubsequence(t, sub)
    }
     */
  }

}

object TreeExercises {
  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    // Exercise 3.25
    def size[A](tree: Tree[A]): Int = tree match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

    // Exercise 3.26
    def maximum(tree: Tree[Int]): Int = tree match {
      case Leaf(i) => i
      case Branch(l, r) => maximum(l) max maximum(r)
    }

    // Exercise 3.27
    def depth[A](tree: Tree[A]): Int = tree match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }

    // Exercise 3.28
    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
      case Leaf(value) => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }

    // Exercise 3.29
    def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
      case Leaf(v) => f(v)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

    def foldSize[A](tree: Tree[A]): Int =
      fold(tree)(_ => 1)(1 + _ + _)

    def foldMaximum(tree: Tree[Int]): Int =
      fold(tree)(a => a)(_ max _)

    def foldDepth[A](tree: Tree[A]): Int =
      fold(tree)(_ => 0)(1 + _ max _)

    def foldMap[A, B](tree: Tree[A])(f: A => B): Tree[B] =
      fold(tree)(a => Leaf(f(a)): Tree[B])((l, r) => Branch(l, r))
  }
}