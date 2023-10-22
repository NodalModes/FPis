Examples of side effects:
- modifying a variable
- modifying a data structure in place
- setting a field on an object
- throwing an exception or halting with an error
- printing to the console or reading user input
- reading from or writing to a file
- drawing on the screen

Scala `case class` is just like (and inspired) Java `record`
```scala
case class Charge(cc: CreditCard, amount: Double) {
  
}
```

`if` **expressions** always return a value

Terminology: function ---> pure, procedure ---> may have side effects 

# Referential Transparency and Purity
> An expression `e` is _referentially transparent_ if, for all programs `p`,
> all occurrences of `e` in `p` can be replaced by the result of evaluating `e`
> without affecting the meaning of `p`.
> A function `f` is _pure_ if the expression `f(x)` is referentially transparent
> for all referentially transparent `x`.

