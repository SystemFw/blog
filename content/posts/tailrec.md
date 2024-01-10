---
title: "Deriving Tail Recursive Fibonacci"
date: 2024-01-11
---

## Introduction

One of the cornerstones of Functional Programming is the use of
_structural recursion_: writing functions that are _recursive_ (they
call themselves) and that model the recursion after the structure of
the data they are operating on.

For example, given the standard `Cons` list:
```scala
sealed trait List[+A]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
case object Nil extends List[Nothing]
```

we can write a `length` function using structural recursion:

```scala
def length[A](list: List[A]): Int =
  list match {
    case Nil => 0
    case Cons(_, tail) => 1 + length(tail)
  }
```

Most introductory material will then explain how certain functions are
_tail-recursive_ if and only if the recursive call is in _tail
position_ , or in other words if and only if they call themselves
recursively as their final action.

Note that looks can be deceiving: although _visually_ it looks like
the recursive call in our `length` function above is in tail position,
it is not, as we can see when we expand the execution trace of its
recursive case:

```scala
case Cons(_, tail) => 1 + length(tail)
// is evaluated like:
val a = 1
val b = length(tail) // not in tail position!
a + b
```

Instead, a tail-recursive `length` looks like this:
```scala
def length[A](list: List[A], acc: Int = 0): Int =
  list match {
    case Nil => acc
    case Cons(_, tail) => length(tail, acc + 1)
  }
```

Tail-recursive functions tend to be less obvious for most people than
their structural counterpart, so you might be wondering whether
learning how to write them is worth the effort.

Well, there are at least of couple of good, practical reasons:

1. The Scala compiler can run tail recursive functions in constant
   stack space, without incurring a `StackOverflowException` if the
   recursion is too deep.
2. Some functions, when written with structural recursion, exhibit
   exponential complexity and are therefore prohibitively slow, and they
   can be made to run in linear time written with tail-recursion.
   
...but I have to be honest, if those were the only reasons, I probably
wouldn't bother writing a post about it.

Instead, I believe the main reason tail-recursion is worth learning is
because the shape of tail-recursive functions forces us to focus on
the _essential state_ needed to perform a certain computation, and
the usefulness of identifying essential state extends far beyond just
Functional Programming. For example, it is crucial in my day job when
dealing with concurrent and distributed algorithms.

This post is divided into two parts: Part I will show a recipe to
derive tail-recursive functions via _algebraic manipulation_, which is
very useful if you know the recursive structure of your computation,
but struggle to express it in tail-recursive terms. Part II will
instead show an alternative method which focuses on _state_, and gets
us closer to the skillset needed to model imperative, and ultimately
concurrent and distributed computations.

## Naive Fibonacci

The running example for this post will be a function to compute the
`N-th` number of the Fibonacci sequence:

> 0, 1, 1, 2, 3, 5, 8, 13, 21, ...

which is described by the following _recurrence relation_:


> F(0) = 0  
> F(1) = 1  
> F(n) = F(n - 1) + F(n - 2)

We can easily translate that into Scala via structural recursion,
noting that it has two base cases, and that we will pretend that
Scala's `Int` cannot be negative:

```scala
def fib(n: Int): Int =
 n match {
   case 0 => 0
   case 1 => 1
   case n => fib(n - 1) + fib(n - 2)
 }
```

Fibonacci is often used as a motivating example to learn
tail recursion because the version we've just written is exponential,
and starts getting pretty slow as we increase `n`.

So, tail recursion to the rescue!

## Algebraic derivation

Let's start from the naive fibonacci:

```scala
def fib(n: Int) = n match {
  case 0 => 0
  case 1 => 1
  case n =>
   def go(): Int = ???
   ???
}
```

As you can see, we have left the recursive case unspecified, and
instead introduced a helper function called `go`, which is where all
the actual recursion is going to happen.
This is a very common style since tail-recursive functions tend to
need additional parameters, which are hidden by the helper function.

In particular, each component of the recursive definition becomes a
parameter, so with the recurrence relation:

> F(n) = F(n - 1) + F(n - 2)

we will have parameters for `n`, `F(n)`, `F(n - 1)`, and `F(n - 2)`.

We can find appropriate names for these parameters by putting them on
a number line in order:

```shell
         F(n - 2)        F(n - 1)          F(n)            n
-------------|---------------|---------------|-------------> 
        secondLast         last          current        counter
```

which means our function becomes:


```scala
def fib(n: Int) = n match {
  case 0 => 0
  case 1 => 1
  case n =>
   def go(secondLast: Int, last: Int, current: Int, counter: Int): Int = ???
   ???
}
```

In order to call `go`, we need to find appropriate initial values for
its arguments: the initial value for `counter` is 2, because
iterations 0 and 1 are already covered by the base cases.
Then, the other params are given by the Fibonacci series up to iteration 2:

> 0, 1, 1, ...

so:

```scala
def fib(n: Int) = n match {
  case 0 => 0
  case 1 => 1
  case n =>
   def go(secondLast: Int, last: Int, current: Int, counter: Int): Int = ???
   go(secondLast = 0, last = 1, current = 1, counter = 2)
}
```


Now let's write `go`. Our `fib` function has to return the value of
the Fibonacci sequence at iteration `n`, or in other words, the
`current` value if `counter` is equal to `n`.

If `counter` is not at `n`, then we advance, so `last` becomes
`secondLast`, `current` becomes `last`, and we compute the next value
of `current` with the definition of Fibonacci, which says the
`current` Fibonacci number is equal to the sum of the `last` and
`secondLast` Fibonacci numbers.

Note that because we have replaced recursive components with arguments
to `go`, we can compute the next value of `current` without any
recursion, we only need to recur in tail position with the updated
values of the arguments to `go`:


```scala
def fib(n: Int) = n match {
  case 0 => 0
  case 1 => 1
  case n =>
   def go(secondLast: Int, last: Int, current: Int, counter: Int): Int =
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

   go(secondLast = 0, last = 1, current = 1, counter = 2)
}
```

and we're done! We can apply several transformations to considerably
simplify this function, but before we do that, let's recap the two
ideas we've used to write tail-recursive functions:

- Every component of the recursive definition becomes an argument to a
  recursive helper.
- At the end of the recursion, one of the arguments will hold the
  final result.
  
One final observation is that the recursion is no longer _structural_,
i.e. the fact that the `current` Fibonacci number is defined
recursively no longer corresponds to recursion in code. Instead, the
logical recursion corresponds to state updates, and the recursion in
code is only used to iterate our state transformations until we reach
the desired results. This is the reason why the recursive call can be
in tail position, but I dare say the link between recursive
definitions and state is a far more interesting aspect of this type of
function than the use of tail-recursion in itself.

### Simplification

The first transformation we can apply is _inlining_, i.e. replacing
each name with its definition.
This is safe to do because even though the algorithm is conceptually
evolving variables, it's expressed as immutable transformations for which
inlining can always be done without changing behaviour.
As an example:
```scala
go(..., currentNext, ...)
// but:
currentNext = lastNext + secondLastNext
// so:
go(..., lastNext + secondLastNext, ...)
// but:
lastNext = current
secondLastNext = last
// so:
go(..., current + last, ...)
```
Let's inline all the definitions in `go`:

```scala
def fib(n: Int) = n match {
  case 0 => 0
  case 1 => 1
  case n =>
   def go(secondLast: Int, last: Int, current: Int, counter: Int): Int =
     if (counter == n) current
     else 
       go(
        counter = counter + 1,
        secondLast = last,
        last = current,
        current = current + last
       )
   
   go(secondLast = 0, last = 1, current = 1, counter = 2)
}
```

but now we notice that `secondLast` is redundant, because it's updated
but never actually used to compute `current`: `current` is updated via
`current + last`, and `last` doesn't depend on `secondLast`.

So we can eiminate it to get to:
```scala
def fib(n: Int) = n match {
  case 0 => 0
  case 1 => 1
  case n =>
   def go(last: Int, current: Int, counter: Int): Int =
     if (counter == n) current
     else 
       go(
        counter = counter + 1,
        last = current,
        current = current + last
       )

   go(last = 1, current = 1, counter = 2)
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
