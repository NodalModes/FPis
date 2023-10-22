package Chapter3

import org.scalatest.funsuite.AnyFunSuiteLike
import Chapter3.ListExercises.List
import Chapter3.TreeExercises._

class E3p9Test extends AnyFunSuiteLike {
  val list: List[Int] = List(1, 2, 3, 4, 5)
  println("test length:\t" + List.length(list))

  println("test sumLeft:\t" + List.sumLeft(list))
  println("test prodLeft:\t" + List.productLeft(list))
  println("test lengthLeft:\t" + List.lengthLeft(list))

  println("test reverse:\t" + List.reverse(list))

  val stringList = List("one", "two", "three")
  println("right:\t" + List.foldRight(stringList, "zero")("(" + _ + ")>[" + _ + "]"))
  println("left:\t" + List.foldLeft(stringList, "zero")("(" + _ + ")>[" + _ + "]"))

  val revStringList = List.reverse(stringList)
  println("right:\t" + List.foldRight(revStringList, "zero")("(" + _ + ")>[" + _ + "]"))
  println("left:\t" + List.foldLeft(revStringList, "zero")("(" + _ + ")>[" + _ + "]"))

  println("ritol:\t" + List.foldRightInTermsOfLeft(stringList, "zero")("(" + _ + ")>[" + _ + "]"))

  println("append:\t" + List.append(stringList, List("four", "five")))
  println("append:\t" + List.append(List(7, 6, 5, 4), List(3, 2)))

  println("concat:\t" + List.concat(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))))

  val listOfInts: List[Int] = List(1, 2, 3, 4, 5)
  println(listOfInts)
  println("vsi:\t" + List.vectorStyleIncrement(listOfInts))
  println(listOfInts)

  val ld: List[Double] = List(0.1, 0.2, 0.3)
  println("dts:\t" + List.doubleToString(ld))

  println("map:\t" + List.map(ld)(a => 10 * a))

  println("filter:\t" + List.filter(listOfInts)(i => i <= 3))

  println("flatMap:\t" + List.flatMap(stringList)(a => List(a.charAt(0), a.charAt(1))))

  println("filterViaFlatMap:\t" + List.filterViaFlatMap(listOfInts)(i => i >= 3))

  println("addElementWise:\t" + List.addElementWise(List(1, 2, 3, 4), List(4, 5, 6, 6, 6, 6)))

  //  one two three ------ 1 2 3 4 5
  // 3 3 5 --------- 1 2 3 4 5
  // 4 5 8 4 5
  println("zipWith:\t" + List.zipWith(stringList, listOfInts, "", 0)((a: String, b: Int) => a.length + b))
  println("TRzipWith:\t" + List.tailRecZipWith(stringList, listOfInts)((a: String, b: Int) => a.length + b))

  println("hasSubsequence:\t ; \t" + List.hasSubsequence(List(), List()))
  println("hasSubsequence:\t1 ;\t" + List.hasSubsequence(List(1), List()))
  println("hasSubsequence:\t; 1\t" + List.hasSubsequence(List(), List(1)))
  println("hasSubsequence:\t1, 2 ; 1\t" + List.hasSubsequence(List(1, 2), List(1)))
  println("hasSubsequence:\t1, 2 ; 1, 2\t" + List.hasSubsequence(List(1, 2), List(1, 2)))
  println("hasSubsequence:\t1, 2; 2, 1\t" + List.hasSubsequence(List(1, 2), List(2, 1)))
  println("hasSubsequence:\t1, 2 ; 1, 2, 3\t" + List.hasSubsequence(List(1, 2), List(1, 2, 3)))
  println("hasSubsequence:\t1, 2, 3 ; 1, 2\t" + List.hasSubsequence(List(1, 2, 3), List(1, 2)))
  println("hasSubsequence:\t1, 2, 3 ; 2, 3\t" + List.hasSubsequence(List(1, 2, 3), List(2, 3)))
  println("hasSubsequence:\t1, 2, 3, 4, 5 ; 3, 4\t" + List.hasSubsequence(List(1, 2, 3, 4, 5), List(3, 4)))
}

class E3p25Test extends AnyFunSuiteLike {
  val tree: Tree[Int] = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
  println(Tree.size(tree))
}