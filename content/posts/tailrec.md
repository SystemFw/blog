---
title: "Deriving Tail Recursive Fibonacci"
date: 2024-01-04
---

Here's a derivation for a tail recursive fibonacci function.

So, which mindset should you use?
The truth is that both are useful for this task, and both are also
useful in general:
equational reasoning comes up when refactoring FP code a lot, whereas
being able to systematically identify essential state is useful when
developing concurrent and distributed algorithms.

## derivation

let's start from the Maths definition:
```
fib(0) = 0
fib(1) = 1
fib(n) = fib(n - 1)  + fib(n - 2)
```

Let's translate the structure, leaving the recursive case unspecified:

```scala
def fib0(n: Int) = n match {
  case 0 => 0
  case 1 => 1
  case n =>
   // fib(n) = fib(n - 1) + fib(n - 2)
   ???
}
```


the recursive definition is made of 4 components: 
  > n, fib(n), fib(n - 1), fib(n - 2)
let's call them:
  > counter, current, last, secondLast
and order them according to how they are ordered visually:
  > counter, secondLast, last, current


Each component becomes an argument to our tailrec helper:

```scala
def fib1(n: Int) = n match {
  case 0 => 0
  case 1 => 1
  case n =>
   def go(counter: Int, secondLast: Int, last: Int, current: Int): Int = ???
   ???
}
```


Now let's think about the starting state, we know that counter
needs to start at 2 because 0 and 1 are already covered by the
previous cases.
The rest follows by how Fibonacci looks.
The starting state is what we call `go` with.

```scala
def fib2(n: Int) = n match {
  case 0 => 0
  case 1 => 1
  case n =>
   def go(counter: Int, secondLast: Int, last: Int, current: Int): Int = ???
   go(counter = 2, secondLast = 0, last = 1, current = 1)
}
```

Now let's write `go`:
If counter is at `n`, we are in the right spot, so we can return `current`.
If not, we update all variables according to the definition of
Fibonacci, and recur.

```scala
def fib3(n: Int) = n match {
  case 0 => 0
  case 1 => 1
  case n =>
   def go(counter: Int, secondLast: Int, last: Int, current: Int): Int = {
     if (counter == n) current
     else 
       val counterNext = counter + 1
       val secondLastNext = last // we move to the right
       val lastNext = current // we move to the right
       val currentNext = lastNext + secondLastNext // definition of Fibonacci
       go(
        counter = counterNext,
        secondLast = secondLastNext,
        last = lastNext,
        current = currentNext
       )
   }
   go(counter = 2, secondLast = 0, last = 1, current = 1)
}
```


Now we are done, but we can simplify by applying inlining,
i.e. we replace each name with its definition in `go`.
This is safe to do because even though the algorithm is conceptually
evolving variables, it's expressed as immutable transformations for which
inlining can always be done without changing behaviour.
As an example:
```scala
go(..., currentNext, ...)
```
but:
```scala
currentNext = lastNext + secondLastNext
```
so:
```scala
go(..., lastNext + secondLastNext, ...)
```
but:
```scala
lastNext = current
secondLastNext = last
```
so:
```scala
go(..., current + last, ...)
```
  
we do the same across the board:

```scala
def fib4(n: Int) = n match {
  case 0 => 0
  case 1 => 1
  case n =>
   def go(counter: Int, secondLast: Int, last: Int, current: Int): Int = {
     if (counter == n) current
     else 
       go(
        counter = counter + 1,
        secondLast = last,
        last = current,
        current = current + last
       )
   }
   go(counter = 2, secondLast = 0, last = 1, current = 1)
}
```


but now we notice that `secondLast` is updated but never actually
used it so we can eliminate it.
To see that, start from the expression that returns the result, which
is `current`.
Then see how `current` is computed, which is with `current + last`,
then see how `last` doesn't depend on `secondLast`, i.e `secondLast`
is never used.


```scala
def fib5(n: Int) = n match {
  case 0 => 0
  case 1 => 1
  case n =>
   def go(counter: Int, last: Int, current: Int): Int = {
     if (counter == n) current
     else 
       go(
        counter = counter + 1,
        last = current,
        current = current + last
       )
   }
   go(counter = 2, last = 1, current = 1)
}
```

Now let's eliminate the named arguments, and hoist the helper at the top:

```scala
def fib6(n: Int) = {
  def go(counter: Int, last: Int, current: Int): Int =
    if (counter == n) current 
    else go(counter + 1, current, current + last)

  n match {
    case 0 => 0
    case 1 => 1
    case n => go(2, 1, 1)
  }
}
```


The pattern matching has redundant logic in the first two cases, and
doesn't use the pattern in the third, we can replace it with an `if`.
`go` is tail recursive, we can make sure by adding an annotation.
This is our final version:

```scala
def fib(n: Int) = {
  @annotation.tailrec
  def go(counter: Int, last: Int, current: Int): Int =
    if (counter == n) current 
    else go(counter + 1, current, current + last)

  if (n <= 1) n else go(2, 1, 1)
}
```

val a = (0 to 8).map(fib)
// res0: Vector[Int]  = Vector(0, 1, 1, 2, 3, 5, 8, 13, 21)


## Alternative derivation 


The derivation above uses equational thinking a lot, but often with
tail recursion we can apply more operational thinking.

Tail recursion can be understood as a translation from mutable state
algorithms, except with a more explicit explicit representation of
imperative thinking, where for each variable `x` you distinguish the
updated version x' from x (due to immutability), and you loop via a
restricted GOTO (the recursive call) instead of using `while`.

To derive tail recursive functions with this type of thinking, the
guiding principle is generally trying to figure out which state you
need to keep track of.
 
Let's start from the Maths definition again:
```scala
fib(0) = 0
fib(1) = 1
fib(n) = fib(n - 1)  + fib(n - 2)
```

This time, we start by recognising the first two cases can be done
with an `if`, and then we know there is going to be some iteration
after, which we can represent with a helper. At this point, the only
information we have about the helper is its return type:

```scala
def fib0(n: Int) = {
  def go(): Int = go()
  if (n <= 1) n else go()
}
```


tail recursive helpers have to return a result which is updated
during the iteration, so we can start by keeping track of that. Note
that to do that, we have to figure out what its initial value should
be. In this case, because the `if` arleady covers 0 and 1, the value
is 1.

```scala
def fib1(n: Int) = {
   def go(result: Int): Int = go(result)
   if (n <= 1) n else go(1)
}
```


Now we have to figure out when it's possible to return `result`,
i.e. we have to to figure out a termination condition.
Sometimes this can be done as a predicate on `result`, requiring no
additional state, but in this case the result has to be returned once
we reach the nTh iteration, which means we have to add some state to
keep track of that with a `counter: Int`.
Similarly, we have to figure out the initial value of `counter`,
because the `if` in `fib2` covers iterations 0 and 1, the value is 2.

```scala
def fib2(n: Int) = {
  def go(counter: Int, result: Int): Int =
    if (counter == n) result
    else go(counter, result)
  if (n <= 1) n else go(counter = 2, result = 1)
}
```


Now we have some state with initial values, and we have to figure out
how to update it before recurring.

```scala
def fib3(n: Int) = {
  def go(counter: Int, result: Int): Int =
    if (counter == n) result
    else {
      val counterNext = counter + 1
      val resultNext = ???
      go(counterNext, resultNext)
    }
  if (n <= 1) n else go(counter = 2, result = 1)
}
```

Updating counter is easy, but we don't know what `resultNext` should
be. According to the definition of fibonacci, it's the sum of `fib(n -
1)` and `fib(n - 2)`. We already have the result of `fib(n - 1)`, it's
`result`!

```scala
def fib4(n: Int) = {
  def go(counter: Int, result: Int): Int =
    if (counter == n) result
    else {
      val counterNext = counter + 1
      val resultNext = result // + something
      go(counterNext, resultNext)
    }
  if (n <= 1) n else go(counter = 2, result = 1)
}
```

However, we don't have info about `fib(n - 2)`, which means we are
missing some state to track it, let's call it `last`.
Understanding why this is the `last` value can be a bit tricky:
remember that we need `fib(n - 2)` in the _next_ iteration, which
means we need `fib(n - 1)` in this iteration, i.e. the last value.

Once you understand that, the rest is easy: `lastNext` is the same as
`result` now, and the initial value of `last` is `fib(n - 1)` when `n
== 2`, i.e. `1

At any let's call it `last`.
The initial value of `old` is `fib(n - 2)` when `n = 2`, i.e. 0

```scala
def fib5(n: Int) = {
  def go(counter: Int, last: Int,  result: Int): Int =
    if (counter == n) result
    else {
      val counterNext = counter + 1
      val resultNext = result + last
      val lastNext = result
      go(counterNext, lastNext, resultNext)
    }
  if (n <= 1) n else go(counter = 2, last = 1, result = 1)
}
```


Again, we can apply some inlining and remove name params, and we're
done:

```scala
def fib(n: Int) = {
  def go(counter: Int, last: Int, result: Int): Int = 
    if (counter == n) result
    else go(counter + 1, result, result + last)

  if (n <= 1) n else go(2, 1, 1)
}
```
