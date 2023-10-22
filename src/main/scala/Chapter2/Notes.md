# Java hates this man
(function inside a function, baby)
```scala
def factorial(n: Int): Int = {
  def go(n: Int, acc: Int): Int = {
    if (n <= 0) acc
    else go(n - 1, n * acc)
  }
  
  go(n, 1)
}
```
# Recursion Convention
"Here, we're defining a recursive helper function inside the body of the `factorial` function.
Such a helper function is often called `go` or `loop` by convention."

# Tail Recursion
In order to make sure that tail recursion optimization has happened, you can use the
`annotation.@tailrec` annotation like this:
```scala
import scala.annotation.tailrec;

def factorial(n: Int): Int = {
  @tailrec
  def go(n: Int, acc: Int): Int = {
    if (n <= 0 ) acc
    else go(n - 1, n * acc)
  }
  
  go(n, 1)
}
```

# Monomorphic Functions: functions that operate on only one type of data
# Parametric Polymorphic Functions: (generics)
```scala
def findFirst[A](as: Array[A], p: A => Boolean): Int = {
  @annotation.tailrec
  def loop(n: Int): Int = {
    if (n >= as.length) -1
    else if (p(as(n))) n
    else loop(n + 1)
  }

  loop(0)
}
```

# Anonymous Functions
```scala
(x: Int) => x == 9
(x: Int, y: Int) => x == y
```

# Functions as values in Scala
"
> When we define a function literal, 
> what is actually being defined in Scala is an 
> object with a method called `apply`. 
> Scala has a special rule for this method name, 
> so that objects that have an `apply` method can be called 
> as if they were themselves methods.

"

# 2.6 Following types to Implementations (WTF)
```scala
def partial1[A, B, C](a: A, f: (A, B) => C) : B => C = {
  b => f(a, b)
}
```
The only way that we know how to get something of type C
is through using the function `f`.
Therefore, this is the only possible implementation.
Also, note that the type of `b` is inferred to be `B` because 
we're expecting a return type of `B => C` from the signature.

This sort of thing stinks of _combinator_.