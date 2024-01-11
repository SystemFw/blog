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
   can be made to run in linear time when written with tail recursion.
   
...but I have to be honest, if those were the only reasons, I probably
wouldn't bother writing a post about it.

Instead, I believe the main reason tail recursion is worth learning is
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

## Part I: Algebraic Derivation

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
recursion, we only need to recur in tail position by passing the
updated arguments to `go`:


```scala
def fib(n: Int) = n match {
  case 0 => 0
  case 1 => 1
  case n =>
   def go(secondLast: Int, last: Int, current: Int, counter: Int): Int =
     if (counter == n) current
     else {
       val counterNext = counter + 1
       val secondLastNext = last // we move to the right
       val lastNext = current // we move to the right
       val currentNext = lastNext + secondLastNext // definition of Fibonacci
       go(
        secondLast = secondLastNext,
        last = lastNext,
        current = currentNext,
        counter = counterNext
       )
     }

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
function than the use of tail recursion in itself.

### Simplification

Ok, now onto simplifying our implementation: the first transformation
we can apply is _inlining_, i.e. replacing each name with its
definition.This is safe to do because even though the algorithm is
conceptually evolving variables, it's expressed as immutable
transformations for which inlining can always be done without changing
behaviour. As an example:
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
        secondLast = last,
        last = current,
        current = current + last,
        counter = counter + 1,
       )
   
   go(secondLast = 0, last = 1, current = 1, counter = 2)
}
```

but now we notice that `secondLast` is redundant, because it's updated
but never actually used to compute `current`: `current` is updated via
`current + last`, and `last` doesn't depend on `secondLast`.

So we can eliminate it to get to:
```scala
def fib(n: Int) = n match {
  case 0 => 0
  case 1 => 1
  case n =>
   def go(last: Int, current: Int, counter: Int): Int =
     if (counter == n) current
     else 
       go(
        last = current,
        current = current + last
        counter = counter + 1
       )

   go(last = 1, current = 1, counter = 2)
}
```

Now let's eliminate the named arguments, and hoist `go` at the top.
It's tail recursive, and we can make sure by adding the `tailrec`
annotation:

```scala
import annotation.tailrec

def fib(n: Int) = {
  @tailrec
  def go(last: Int, current: Int, counter: Int): Int =
    if (counter == n) current 
    else go(current, current + last, counter + 1)

  n match {
    case 0 => 0
    case 1 => 1
    case n => go(1, 1, 2)
  }
}
```


Finally, we can see that the pattern matching has redundant logic in
the first two cases, and doesn't use the pattern in the third, so we
can replace it with an `if`. This is our final version:

```scala
import annotation.tailrec

def fib(n: Int) = {
  @tailrec
  def go(last: Int, current: Int, counter: Int): Int =
    if (counter == n) current 
    else go(current, current + last, counter + 1)

  if (n <= 1) n else go(1, 1, 2)
}
```

## Part II: Operational Derivation 

The derivation above used a lot of equational thinking, but often with
tail recursion we can adopt a more operational mindset.

In fact, tail recursion can be understood as a translation from
mutable state algorithms, one where imperative thinking is more
explicit than in most imperative languages, in that for each variable
`x` you distinguish `x` from its updated version `x'` (due to
immutability), and you loop via a restricted `GOTO` (the recursive
call) instead of using `while`.

So, let's look at the recurrence relation again:

> F(0) = 0  
> F(1) = 1  
> F(n) = F(n - 1) + F(n - 2)


This time, we start by recognising that the first two cases can be
done with an `if`, and then we know there is going to be some
iteration in the recursive case, which we can represent with a helper.
At this point, the only information we have about the helper is its
return type:

```scala
def fib(n: Int) = {
  def go(): Int = go()
  if (n <= 1) n else go()
}
```

The guiding principle when deriving tail-recursive functions via
operational thinking is figuring out which state you need to keep
track of, and add each piece of state as a parameter.
Tail recursive helpers have to return a result which is updated
during the iteration, so we can start by keeping track of that.

```scala
def fib(n: Int) = {
  def go(result: Int): Int = go(result)
  if (n <= 1) n else go(???)
}
```

To call `go`, we need to figure out the initial value of `result`. We
know that the `if` returns the result of `fib(n)` when `n == 0` and `n
== 1`, so the initial value of `result` is `fib(n)` when `n == 2`,
which is 1, as per:

> 0, 1, 1, ...

```scala
def fib(n: Int) = {
   def go(result: Int): Int = go(result)
   if (n <= 1) n else go(1)
}
```

Next, we have to figure out when it is possible to return `result`,
i.e. a _termination condition_. Sometimes this can be done as a
predicate on `result` without any additional state, but in this case
the result has to be returned once we reach the `n-th` iteration,
which means we have to add a `counter: Int` parameter to keep track of
which iteration we're in.

Because the `if` covers iterations 0 and 1, so the initial value of
`counter` will be 2:

```scala
def fib(n: Int) = {
  def go(result: Int, counter: Int): Int =
    if (counter == n) result
    else go(result, counter)
  if (n <= 1) n else go(result = 1, counter = 2)
}
```


Now that we have some state with initial values, we have to figure out
how to update it before recurring:

```scala
def fib(n: Int) = {
  def go(result: Int, counter: Int): Int =
    if (counter == n) result
    else {
      val counterNext = counter + 1
      val resultNext = ???
      go(resultNext, counterNext)
    }
  if (n <= 1) n else go(result = 1, counter = 2)
}
```

Updating `counter` is trivial, but we don't know what `resultNext`
should be. Well, according to the definition of Fibonacci, it's the
sum of `fib(n - 1)` and `fib(n - 2)`, and we already have `fib(n -
1)`, it's `result`!

To see why, consider that `resultNext` is the `result`, i.e. the
`fib(n)`, of the _next_ iteration, which means that `fib(n - 1)` is
the previous value from the point of view of the next iteration, i.e
the `result` of the current iteration. Let's update the code:

```scala
def fib(n: Int) = {
  def go(result: Int, counter: Int): Int =
    if (counter == n) result
    else {
      val counterNext = counter + 1
      val resultNext = result + ???
      go(resultNext, counterNext)
    }
  if (n <= 1) n else go(result = 1, counter = 2)
}
```

However, we simply cannot compute `fib(n - 2)`, which means we are
missing a parameter to track it, let's call it `last: Int`.
Understanding why this is the `last` value can be a bit tricky, but
it's the same idea as before: we need `fib(n - 2)` in the _next_
iteration, which means we need `fib(n - 1)` in this iteration, i.e.
the last value.

Now, `last` is going to need an initial value, and it's also going to
need to be updated on each iteration before the recursive call.
Let's start with the initial value, which is straightforward: we need
`fib(n - 1)` when `counter == 2`, which is 1.
The updated `lastNext` value represents, once again, the previous
result from the point of view of the _next_ iteration, i.e. the
`result` of the current iteration.

So we have:

```scala
def fib(n: Int) = {
  def go(result: Int, counter: Int, last: Int): Int =
    if (counter == n) result
    else {
      val counterNext = counter + 1
      val resultNext = result + last
      val lastNext = result
      go(resultNext, counterNext, lastNext)
    }
  if (n <= 1) n else go(result = 1, counter = 2, last = 1)
}
```

And let's apply the same refactoring as in Part I:

```scala
import annotation.tailrec

def fib(n: Int) = {
  @tailrec
  def go(result: Int, counter: Int, last: Int): Int =
    if (counter == n) result
    else go(result + last, counter + 1, result)

  if (n <= 1) n else go(1, 2, 1)
}
```

## Conclusion

Although most didactic material covers the idea of tail recursion,
actually _writing_ tail-recursive functions is often left as an
exercise to the reader.

This is a shame because, far from just being an optimisation, tail
recursion is actually great at expressing tricky stateful logic, and
really teaches us to think about state methodically.

So, tail recurse more, and see you next time!

