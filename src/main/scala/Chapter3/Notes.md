# Empty List
```scala
List()
Nil
```

# List Concatenation
```scala
a ++ b
```

One should define a `toString` method with recursive data types like lists
in order to avoid stack overflows on them when they're large. 

# Covariance (Variance)
```scala
sealed trait List[+A]
```
Here, `+A` denotes covariance, which means that 
**a `List` of something that is a subtype of (extends) `A` is a subtype of (extends) a `List` of `A`** (I think).
`List[+A] means List[a <: A] <: List[A] (I think)`

# Variadic Functions
```scala
def apply[A](as: A*): List[A] =
  if (as.isEmpty) Nil
  else Cons(as.head, apply(as.tail: _*))
```
When defining a datatype, by convention it should have an `apply` in its companion object.
This allows for literal syntax like `List(1, 2, 3, 4)`. 
`A*` is syntactic sugar for `Seq[A]`.
`Seq` has the functions `head` and `tail`.
Here, `_*` is used in order to pass a `Seq` back into the function, which is a variadic method.

# Grouping Arguments (Currying)
## Definition
### Original: 
```scala
def dropWhile[A](l: List[A], f: A => Boolean): List[A] = ???
```
### Curried:
```scala
def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = ???
```
## Call
### Original
Type of anonymous function must be annotated
```scala
val xs: List[Int] = List(1,2,3,4,5)
val ex1 = dropWhile(xs, (x: Int) => x < 4)
```
### Curried
type of anon function can be inferred because `dropWhile` first accepts the `List[Int]`,
pinning the type `A` to `Int`. 
"Type information moves from left to right." 
(If the function was the first argument instead of the list, 
then the type of the function might have to be specified, but the type of the list might be able to be inferred).
```scala
val xs: List[Int] = List(1,2,3,4,5)
val ex1 = dropWhile(xs)(x => x < 4)
```
Apparently this is a restriction of the Scala compiler. 
I don't know if that means a permanent restriction due to the nature of the language itself,
or if this restriction might be outdated in Scala 3.

# Underscore notation for anonymous functions
`_ + _      ====    (x,y) => x + y`
`_ * 2      ====    x => x * 2`
`_.head     ====    xs => xs.head`
`_ drop _   ====    (xs, n) => xs.drop(n)`

# Cons in standard Lib
`Cons(1, Cons(2, Cons(3, Nil))) ==== 1 :: 2 :: 3 :: Nil ==== 1 :: (2 :: (3 :: Nil)) ==== List(1, 2, 3)`
"all methods whose names end in `:` are right-associative"
====
"`x :: xs` is `x.::(xs)` which calls the data constructor `::(x, xs)`"

