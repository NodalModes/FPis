# THUNK
The unevaluated form of an expression is called a **thunk**,
and we can _force_ the **thunk** to evaluate the expression and
get a result.

# Formal definition of strictness
If the evaluation of an expression runs forever or throws an error 
instead of returning a definite value,
we say that the expression doesn't _terminate_, 
or that it evaluates to _bottom_.
A function `f` is _strict_ if the expression `f(x)` evaluates to bottom
for all `x` that evaluate to bottom.

# By name vs by value
A non-strict function takes its arguments by name rather than by value.

# Stream is deprecated (use LazyList instead)

# Corecursion
> Whereas a recursive function consumes data, 
> a corecursive function _produces_ data.
> And whereas recursive functions terminate by recursing on smaller inputs, 
> corecursive functions need not terminate as long as they remain _productive_,
> which just means that we can always evaluate more of the result in a finite
> amount of time. 
> Corecursion is also sometimes called _guarded recursion_,
> and productivity is also sometimes called _cotermination_.