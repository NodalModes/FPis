package Chapter4

import Chapter3.ListExercises.Cons




object Exercises {

  sealed trait Option[+A] {

    // Exercise 4.1
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(a) => Some(f(a))
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(a) => a
    }

    def flatMap[B](f: A => Option[B]): Option[B] =
      map(f).getOrElse(None)

    def flatMapPat[B](f: A => Option[B]): Option[B] = this match {
      case None => None
      case Some(a) => f(a)
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] =
      map(Some(_)).getOrElse(ob)

    def orElsePat[B >: A](ob: => Option[B]): Option[B] = this match {
      case None => ob
      case _ => this
    }

    def filter(f: A => Boolean): Option[A] =
      flatMap(a => if (f(a)) Some(a) else None)

    def filterPat(f: A => Boolean): Option[A] = this match {
      case Some(a) if f(a) => this
      case _ => None
    }

    // Exercise 4.2
    def mean(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None else Some(xs.sum / xs.length)

    def variance(xs: Seq[Double]): Option[Double] = {
      mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
    }
  }

  object Option {
    def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

    // Exercise 4.3
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
      a.flatMap(aa => b.map(bb => f(aa, bb)))
    }

    // Exercise 4.4
    def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
      case Nil => Some(Nil)
      case head :: next => head.flatMap(h => sequence(next).map(h :: _))
    }

    // Exercise 4.5

    import Chapter3.ListExercises.List

    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
      List.foldRight[A, Option[List[B]]](a, Some(List()))((aa, olb) => map2(f(aa), olb)(Cons(_, _)))
    }

    def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] = {
      traverse(a)(aa => aa)
    }

  }

  case object None extends Option[Nothing]

  case class Some[+A](get: A) extends Option[A]


  sealed trait Either[+E, +A] {
    // Exercise 4.6
    def map[B](f: A => B): Either[E, B] = this match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE ,B] = this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }
    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(_) => b
      case Right(a) => Right(a)
    }
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this match {
      case Left(value) => Left(value)
      case Right(aValue) => b match {
        case Left(value) => Left(value)
        case Right(bValue) => Right(f(aValue, bValue))
      }
    }

    def map2FC[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      for {
        a <- this
        bb <- b
      } yield {
        f(a, bb)
      }

    // Exercise 4.7
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
      traverse(es)(a => a)
    }

    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
      as.foldRight[Either[E, List[B]]](Right(Nil: List[B]))((a, b) => f(a).map2(b)(_ :: _))
    }
  }

  case class Left[+E](value: E) extends Either[E, Nothing]

  case class Right[+A](value: A) extends Either[Nothing, A]

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch {
      case e: Exception => Left(e)
    }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}
