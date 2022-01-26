---
title: "Programs as Values, Part V: Outputs"
date: 2022-01-10
---

We want to write an algebra that reads from stdin, and use it to write
the following program:

> read a line from stdin, compute its length, and return `true` if the length
> is greater than 10, or `false` otherwise.

A starting point could be:

```scala
/*
 * carrier:
 *   In
 * introduction forms:
 *   readLine: In
 */
sealed trait In {
  ...
object In {
  val readLine: In
  ...
```

but obviously that's not enough to write our program: we have encoded
the action of reading a line from stdin, but we still need to do some
extra transformations on the line we've read.

So, your first instinct might be to add an elimination form:

```scala
/*
 * carrier:
 *   In
 * introduction forms:
 *   readLine: In
 * elimination forms:
 *   nope: In => String
 */
sealed trait In {
  def nope: String
  ...
object In {
  val readLine: In
  ...
```
in order to write:
```scala
val out: Boolean = readLine.nope.length > 10
```

but this is not a fruitful direction: we are basically saying that the
only way to change the output of a program written in the `In` algebra
is to eliminate the algebra entirely.

Algebras are our unit of composition, therefore with the elimination
approach any program that needs to change its output can no longer be
composed, which is a _really_ strong limitation: for example in [Part
III](https://systemfw.org/posts/programs-as-values-III.html) we saw
that for `IO` elimination happens when the JVM calls `main`, it would
be really weird if we couldn't encode something as simple as
`String.length` until then.

Instead, we want to have the ability to transform outputs without
leaving our algebra, and therefore we have to enrich it with a
`transformOutput` _combinator_. Recall that the general shape of a
combinator is:

```scala
transformOutput: (In, ...) => In
```

and we need to fill the `...` with something that can encode the
idea of transforming one thing into another, which we already have
a well known concept for: functions. So, `transformOutput` needs to take
a function, but we have a problem: what type should this function be,
in order to fit the possible transformations we want to encode such as
`_.length` or `_ > 10` ?

Of course, an `Any => Any` fits anything:

```scala
/*
 * carrier:
 *   In
 * introduction forms:
 *   readLine: In
 * combinators:
 *   transformOutput: (In, Any => Any) => String
 */
sealed trait In {
  def transformOutput(transform: Any => Any): In
  ...
object In {
  val readLine: In
  ...
```

but this is also not an acceptable solution: our algebra has gained
power, but we have lost type safety altogether.

As it turns out, the issue is with the carrier, specifically that
these two programs have the same type:

```scala
val readsLine: In
val computesLength: In
```

which means we cannot link them with a function without casting: we
know that the function to pass to `transformOutput` should have type
`String => Int`, but the compiler doesn't.

```scala
val readsLine: In =
  In.readLine
val computesLength: In
  In.transformOutput(str => str.asInstanceOf[String].length)
```


The key idea out of this problem is that we can add a _type parameter_
to `In` which represents the type of its _output_. The resulting type
`In[A]` lets us write:

```scala
val readsString: In[String]
val computesInt: In[Int]
```

Note that this doesn't require us to actually perform any action,
we're still just building a datatype with a sealed trait and case
classes, except this datatype now carries enough
type information to allow for well typed composition. In other words,
`In[String]` is not a container that contains a `String`, rather it's
a command to eventually read one, encoded as a datatype.

`transformOutput` can now have a proper type:
```scala
def transformOutput[A, B]: (In[A], A => B) => In[B]
```

This signature has two _type variables_ (or _type parameters_), `A`
and `B` . The rule with type variables is that whenever the same type
variable is mentioned, the relative types have to match: in this case,
`(In[A], A => ...` means that the input of the function needs to match
the output of the `In` program, and `... => B) => In[B]` means that
the output of the resulting `In` program will match the output of the
function. Therefore in the example above the function we need to pass
to `transformOutput` to connect `readsString: In[String]` with
`computesInt: In[Int]` has to have type `String => Int`, just like we
expect.

Conversely, whenever _different_ type variables appear, the relative
types _can_ be different, but they don't have to, or in other words
`transformOutput` also works if you use it with an `In[String]` and a
`String => String`, resulting in another `In[String]`.

We can now write a proper version of `In`:

```scala
/*
 * carrier:
 *   In[A]
 * introduction forms:
 *   readLine: In[String]
 * combinators:
 *   transformOutput[A, B]: (In[A], A => B) => In[B]
 */
sealed trait In[A] {
  def transformOutput(transform: A => B): In[B]
  ...
object In {
  val readLine: In[String]
  ...
```

and use it to express our original program:

```scala
val prog: In[Boolean] =
  readLine
    .transformOutput(line => line.length)
    .transformOutput(length => length > 10)
```

Finally, we need to complete `In` with an elimination form so that we
can embed it into bigger programs, as usual we will translate to `IO`:

```scala
/*
 * carrier:
 *   In[A]
 * introduction forms:
 *   readLine: In[String]
 * combinators:
 *   transformOutput[A, B]: (In[A], A => B) => In[B]
 * elimination forms:
 *   run[A]: In[A] => IO[A]
 */
sealed trait In[A] {
  def transformOutput(transform: A => B): In[B]
  def run: IO[A]
  ...
object In {
  val readLine: In[String]
  ...
```

that being said, we won't be thinking about eliminations forms for the
next few articles, as we focus on writing programs _with_ our
algebras. We will return to the topic of elimination forms once we
talk about `IO` in more detail.

## Laws

You might be wondering why I have written the final program as:

```scala
val prog1: In[Boolean] =
  readLine
    .transformOutput(line => line.length)
    .transformOutput(length => length > 10)
```

as opposed to:

```scala
val prog2: In[Boolean] =
  readLine.transformOutput(line => line.length > 10)
```

`prog2` seems less verbose, so should we refactor `prog1` into
`prog2`? Will the behaviour change? Intuitively, it would feel really
weird if it did: transforming the output twice ought to be the same of
transforming it once with the composite transformation.

We can encode this type of assumption as a _law_, something of shape:
```scala
expr1 <-> expr2
```
where `<->` means that `expr1` can be rewritten into `expr2`, and vice versa.
In our case, we will say that:

```scala
p.transformOutput(f).transformOutput(g) <-> p.transformOutput(x => g(f(x)))

where:
  p: In[A]
  f: A => B
  g: B => C
```

which means that we can switch between `prog1` and `prog2` at will,
and not just in the case where `p = readLine`, `f = _.length`, and `g
= _ > 10`, but for _any_ `p`, `f`, and `g`, as long they have the
correct type. So in this case we use laws as a _refactoring aid_ :
they gave us freedom to refactor by specifying which transformations
on our programs are harmless.

By the way, since Scala functions already have an `andThen` method to
express function composition, the law above can be written as:

```scala
p.transformOutput(f).transformOutput(g) <-> p.transformOutput(f.andThen(g))
```

And as it turns out, there is another law concerning
`transformOutput`, the fact that transforming an output with a
function that doesn't change it is the same as not transforming it at
all:

```scala
p.transformOutput(x => x) <-> p

where:
  p: In[A]
```

If this seems completely obvious, that's because it is! Many laws are
just stating: my algebra behaves in the way you expect.

## Conclusion

In this article we introduced a really important idea: encoding the
output of a program by adding a type parameter to the carrier type of
our algebra. This enabled us to add the `transformOutput` combinator,
and next time we will use the same insight to model _chaining_, which
is the essence of sequential control flow.
