---
title: "Programs as Values, Part II : Doing & Being"
date: 2020-07-05
---

A large part of programming in many domains is dominated by
_computational effects_ such as concurrent state or I/O, yet one
often hears that functional programming is about limiting or
eliminating them. As it turns out though, programs as values has a lot
to say about effects, and the common refrain of "having a
non-effectful core with effects at the edges" is not a constraint of
the paradigm, but merely architectural advice, and like all advice it
is sometimes useful, and sometimes harmful. In fact, I like programs
as values _because_ I have to deal with effects, not in spite of it,
and they will be the focus of this series.

Effectful code introduces a few fundamental problems:

- Controlling when an effect happens.
- Changing the meaning of an effect based on contest (e.g. in a test).
- Mixing different effects.

A lot has been written about the last two points, but here I want to
focus on the first, and introduce the notion of __doing__ vs
__being__.

## Doing & Being

Let's look at these two snippets.

```scala
"hello".append(readLine)
```

```scala
repeatAction(times = 3, readLine)
```

The exact definition of `append` and `repeatAction` does not matter
too much, pay attention to the use of `readLine` instead, which is
very different:

- In the first snippet, we want to _execute_ `readLine` and pass its
  _result_ to `append`.
- In the second snippet, we don't want to pass `readLine`'s result to
  `repeatAction`, but `readLine` _itself_, without executing it.
  `repeatAction` can then use it internally as it pleases (in this
  case by executing it 3 times).

I'll call these two modes of use `doing` and `being` respectively.
`doing` is in a sense the essence of effectful computation, i.e.
executing actions and using their results in subsequent actions.
`being` instead describes passing things to functions, giving them
names, returning them as results, or putting them in data structures.
In other words, it describes _values_.

Generally we tend to associate `being` with non-effectful things like
`5` or `"hello"`, but applying this principle to effectful actions
enables **compositional APIs** : `repeatAction` is only possible when
we use `readLine` in the `being` mode.

We can then compare paradigms for effectful computation in terms of
how they approach `doing` vs `being`, but before doing that, let's
define a bit more precisely what it means for something to be a
_value_.

## Referential transparency

Let's say we want to evaluate this simple mathematical expression

```
(x + 3) + (x * 2)  where x = 2
```

what we do is replace `2` wherever we see `x`, yielding

```
(2 + 3) + (2 * 2)
```

to which we then apply reduction rules to get to `9`.

This process is called the __substitution model of evaluation__, since
it's centred around the concept of _substitution_: replacing names
with the expression they refer to. We typically don't think about
programs this way though, using instead a state machine model where
we go line by line and update the relevant state, as shown in this
snippet:

```scala
val x : String = "Hello".reverse // x is "olleh"
val y : String = x ++ x // y is "olleHolleH"
// final result is "olleHolleH"
```

However,  we can try using the substitution model instead (by replacing
`x` with `"Hello".reverse` everywhere), and nothing changes:

```scala
val y : String = "Hello".reverse ++ "Hello".reverse
// final result is "olleHolleH"
```

This property is called __referential transparency__: _replacing an
expression with its bound value does not alter the observable
behaviour of the program_. In other words, an expression is
referentially transparent if it respects the subtitution model of
evaluation. We are going to call such expressions __values__.

Referential transparency (RT from now on) captures the essence of
being a value, which is the fact that values require only _local_
reasoning: no matter where a value is used, it is entirely
characterised by its definition, as evidenced by the fact that you can
entirely replace it with such definition without altering the
behaviour of the program.

On a practical level, RT gives us a _systematic_ way to understand
code: just replace names with definitions until you are at a
sufficient level of detail. In the other direction, it also lets us
_forget_ about the details of a given piece of code: just give that
piece a name, and use that name everywhere without fear of breaking
anything. Inlining and name abstraction are always valid with RT
code, and they are useful refactoring tools, in addition to being
highly compositional.

Let's now give a couple of examples where RT does not hold, starting
by evaluating this snippet in the state machine model:

```scala
// `it` is an iterator over the sequence 1,2,3...
val a = it.next // a is 1
val b = a + 1   // b is 2
a + b // final result is 3
```

if we apply the substitution model, we get this instead

```scala
// we start from here
val a = it.next
val b = a + 1
a + b

// let's replace the definition of b, nothing changes

val a = it.next
a + a + 1

// let's now replace the definition of a

it.next + it.next + 1 // final result is 4
```

The final behaviour is not the same! Let's look at another one:

```scala
val b = {
 println("hello") // "hello" gets printed, () is returned
 println("hello") // "hello" gets printed, () is returned
 3 // final result is 3
}

```

now let's apply the substitution model in reverse by giving a name to
`println("hello")`

```scala
val b = {
 val p = println("hello") // "hello" gets printed, () is returned
 p // () is returned
 p // () is returned
 3 // final result is 3
}
```

Behaviour changes again! We say that `it.next` and `println("hello")`
are RT breaking or, in other words, that they are not values.


> **Note on terminology**
>
> In pure FP, RT breakages are typically called "side-effects", which
> is an unfortunate source of ambiguity.
>
> When advocates of pure FP say they
> "program without side-effects", they mean "without RT breakages", but
> outsiders hear "without computational effects such as I/O", which
> leaves them understandably confused.
>
> In this series we will always
> prefer "programs as values" to "pure FP", and "RT breakage" to "side
> effect".

## Execution as evaluation

We can now describe the effect model used by the vast majority of
languages (Java, Go, JS, C++, Python etc), starting from their
_evaluation strategy_, which I will informally define as the order in
which expressions are reduced.

These languages are based on _strict evaluation_ (or _call by value_),
in which expressions are evaluated as soon as they are bound to a name,
and the arguments of a function are evaluated before the function is
applied, which results in a natural sequential order.

For this reason, these languages treat _execution as evaluation_:
effects are executed during the process of evaluating the expression
in which they are defined, and if we ever want to prevent execution,
we need to _suspend evaluation_. Using the language we have developed
earlier, we can say that execution as evaluation is `doing`-first,
with explicit `being`.

In Scala, the two aspects correspond to different language features,
with `val` and by-value parameters (`: A`) used for `doing`, and `def`
and by-name parameters (`: => A`) used for `being`.

```scala
// Examples of doing

// by-value parameters
def append(s1: String, s2: String) = {
 println("appending")
 s1 ++ s2
}

// readLine is executed before "appending" is printed
val b = append("hello", readLine)

// "appending" is printed before "done appending"
println("done appending")

// the line above can be understood conceptually as
val _ = println("done appending")

//nothing is read here, already happened
b
```

```scala
// Examples of being

// `action` is a by-name parameter 
def repeatN(times: Int, action: => A): List[A] = ???

// nothing is executed here
def readThree = repeatN(3, readLine)

// at some point, it has to be done, using `val`
val readFirstTriplet = readThree

// effects happen again, three more lines are read
val readSecondTriplet = readThree
```

I'm going to postpone discussing the limitations of this approach in
Scala until the Appendix, because we are finally ready to explore the
core idea behind programs as values.

## Programs as values

Programs as values goes in the exact opposite direction of execution
as evaluation. Its key principle is: `being` first, with explicit
`doing`.

Because of this emphasis on `being`, programs written in this paradigm
are composed entirely of values, i.e. referentially transparent
expressions. And what's the most straightforward kind of value? Well,
just datatypes: effectful programs are represented simply as datatypes
which describe the _intention_ of executing a specific effect.

When everything, including effectful programs, is a value,
compositional `being` APIs become easy, just pass datatypes
around, combine them with functions, put them in data structures, and
so on, without worrying about when evaluation happens.

But the question is, how do we represent `doing`? The idea is that
instead of relying on evaluation, we design combinator functions that
describe the intention of running a program after another, i.e. we
represent `doing` explicitly through a `being` api.

In the end, our whole program will be represented by a single expression,
which builds an instance of our effect datatype. This instance can
then be translated into actual computational effects, _once_, at the
so called "end of the world": in Scala this means the `main`
function, whereas Haskell goes one step further by doing the
translation in the runtime system, so that the programmer doesn't even
see it.

This snippet of pseudo code should give you an idea of how things look like in a programs as values codebase:

```scala
// the type of our program
type Console

// constructs Console values
def print(s: String): Console

// simply returns a value of type Console, does not do anything
val hey: Console = print("hey ")

// nothing gets printed, but we return another value of type Console
// that explicitly describes printing one word after another
val p: Console = hey.andThenDo(hey).andThenDo(print("you"))

// Everything is RT, so p is equivalent to
val p: Console = 
  print("hey ")
   .andThenDo(print("hey "))
   .andThenDo(print("you"))

object Main {
  def main(args: Array[String]): Unit =
    p.translateToActualEffects
}
```

and believe it or not, that's the whole idea: we represent effects as
datatypes (i.e. _programs as values_), and we use functions to
assemble big programs out of smaller ones. The rest comes down to
discovering richer datatypes for our effects, and exploring the
structure of the functions we use to combine them. And that is exactly
what we are going to do next time.

---

## Appendix

Although Scala goes further than most mainstream languages in its
support for `being`, there are significant limitations in its
approach.

First of all, by-name parameters (`: => A`) don't have their own type,
which on one hand means that it's not possible to use them as results,
and on the other that mistakenly triggering evaluation too early and
then passing the value will not trigger a type error.

For example:

```scala
val read = readLine
repeatAction(3, read)
```

It's impossible to completely prevent this problem, but a type error
would go a long way in forcing you to think it through. In fact,
having by-name parameters be completely transparent at call site is
less than ideal: in a model where the execution of effects is tied to
evaluation, knowing at a glance what's evaluating when is very
important, and Scala forces you to go to the definition site instead.

You might be thinking that all these issues are easily solved by using
`() => A` instead of `=> A`, but this exposes another problem: tying
the representation of `=> A` to `() => A` is limited to synchronous
execution. As a result, by-name params are mixed with abstractions
that are inspired by programs as values, but are not RT, like `Future`.
The resulting model is the worst of both worlds, since _some_ effects
are controlled by evaluation, and some by explicit combinators like
`flatMap`: you lose both the immediacy of execution as evaluation, and
the systematic clarity of programs as values.

Finally, allowing effects in top level `val`s in the body of classes
and objects sacrifices the property of having the whole program be a
single expression, which has important consequences in the treatment
of mutable state, see my talk
[here](https://systemfw.org/talks.html#scala-italy-2018).

However, it's possible to imagine a model based on execution as
evaluation that does not suffer from these problems. In particular,
models based on **effects and handlers** (also known as algebraic
effects) can not only overcome these issues, but also bring new things
to the table, including more granular effect types, an elegant way to
abstract over concrete effects, and the ability to write advanced
control flow in user code (including async, exceptions, and
nondeterminism).

If you're interested, check out the paper on
[Frank](http://homepages.inf.ed.ac.uk/slindley/papers/frankly-jfp.pdf),
which not only introduces an interesting language based on effects and
handlers, but it's also where the terms `doing` and `being` come from
(although they aren't analysed in depth there, hence why this post).

The [Unison](https://www.unisonweb.org) language is based on a similar
model, and both languages might be the topic of future posts. For now
though, we will deep dive into programs as values instead, see you
next time!
