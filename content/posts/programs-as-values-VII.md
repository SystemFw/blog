---
title: "Programs as Values, Part VII: Exploring Chaining"
date: 2022-01-27
---

Last time we introduced the key concept of _chaining_: creating
programs that can depend on the output of other programs, and can
therefore encode arbitrary sequential control flow. In this shorter
follow-up we will explore some of the properties of chaining that are
relevant when writing real code.

## A rose by any other name

Our main focus has been on three functions: `emitOutput`,
`transformOutput`, and `chain`. Perhaps unsurprisingly given some of
the `IO` snippets I've shown during these series, we can now reveal
those aren't the real names used in [cats](github.com/typelevel/cats)
and [cats-effect](github.com/typelevel/cats-effect). Enter `pure`,
`map`, and `flatMap`:

```scala
emitOutput[A]: A => Console[A]
      pure[A]: A => Console[A]
```
```scala
transformOutput[A, B]: (Console[A], A => B) => Console[B]
            map[A, B]: (Console[A], A => B) => Console[B]
```
```scala
  chain[A, B]: (Console[A], A => Console[B]) => Console[B]
flatMap[A, B]: (Console[A], A => Console[B]) => Console[B]
```

The rationale behind these names is not that important, what matters
is their type and the intent of the programs they return: emitting an
output (`pure`), transforming the output of another program (`map`),
and using the output of a program to determine what the next
program should be (`flatMap`).

Here's `Console` with the standard names in place:

```scala
/*
 * carrier:
 *   Console[A]
 *     where A represents the output of a Console program
 * introduction forms:
 *   readLine: Console[String]
 *   print: String => Console[Unit]
 *   pure[A]: A => Console[A]
 * combinators:
 *   flatMap[A, B]: (Console[A], A => Console[B]) => Console[B]
 *   map[A, B]: (Console[A], A => B) => Console[B]
 * elimination forms:
 *   run[A]: Console[A] => IO[A]
 */
 sealed trait Console[A] {
   def flatMap[B](next: A => Console[B]): Console[B]
   def map[B](transform: A => B): Console[B]

   def run: IO[A]
   ...
 object Console {
   val readLine: Console[String]
   def print(s: String): Console[Unit]
   def pure[A](out: A): Console[A]
   ...
```
and here's our sample program that greets any user that has a non-empty
username:

```scala
def repeatOnEmpty(p: Console[String]): Console[String] =
  p.flatMap { str =>
    if (str.isEmpty) repeatOnEmpty(p)
    else Console.pure(str)
  }

val namePrompt: Console[String] =
  Console
    .print("What's your username? ")
    .flatMap { _ => Console.readLine }

val promptAndGreet: Console[Unit] =
  repeatOnEmpty(namePrompt)
    .map { username => s"Hello, $username!" }
    .flatMap { greeting => Console.print(greeting) }
```

For the rest of the series we will use the same names `cats` uses, so
that knowledge can be transferred immediately.

## map vs flatMap

You might have noticed that there is some similarity between the types of `map` and `flatMap`:

```scala
// map[A, B]:     (Console[A], A =>         B ) => Console[B]
// flatMap[A, B]: (Console[A], A => Console[B]) => Console[B]

trait Console[A] {
  def map[B](transform: A => B): Console[B]
  def flatMap[B](next: A => Console[B]): Console[B]
  ...
```

They both take functions that process the output of a
previous computation, but `flatMap` uses it to determine the next
computation as per the shape `A => Console[B]`, whereas `map` just
transforms it into another value as per the shape `A => B`.

Well, but we said that programs are values, so can we not pass a
function that returns a program to `map`? Let's see what happens by
experimenting with a very simple `echo` program that reads a line and
prints it back, then terminates:

```scala
val echo: Console[Unit] =
  Console
    .readLine
    .flatMap { line => Console.print(line) }
```


and let's replace `flatMap` with `map`:

```scala
val echo2: Console[Console[Unit]] =
  Console
    .readLine
    .map { line => Console.print(line) }
```

`Console.readLine.map[B]` expects a `String => B` and returns a
`Console[B]`, and we're passing a `String => Console[Unit]` to it,
which means that `B = Console[Unit]`, and that the result will have
type `Console[Console[Unit]]`. However when `echo2` runs (via `run`
and `IOApp`) it will read a line from stdin, and then terminate
_without_ printing anything to stdout.

This behaviour happens because `Console[Console[Unit]]` is not a
chained program, but a program that returns another program _as an
output_. The fact that this output also happens to be of type
`Console` doesn't change anything, `.map` treats it like any other
output.

Therefore we need to chain explicitly via `flatMap` for it to run:

```scala
val echo2: Console[Console[Unit]] =
  Console
    .readLine
    .map { line => Console.print(line) }

val echo: Console[Unit] =
  echo2.flatMap { nextProgram => nextProgram }
```

Similarly, the following program will only print "world!", because the
first `Console.print` is also not chained via `flatMap(_ => )`:

```scala
val prog: Console[Unit] = {
  Console.print("Hello ")
  Console.print("world!")
}
```

In practice, using `map` instead of `flatMap` or skipping `flatMap`
altogether are common sources of errors, look out for them whenever
your programs aren't executing something you think they should
execute.

## Laws

Most material about laws is either of theoretical nature, or it talks
about laws as contracts to respect when implementing algebras, but
there's very little talk about them from the _user_'s perspective.
I want to share a practical view of laws as _refactoring rules_ ,
where the equivalence symbol `p1 <-> p2` can be read as `p1 can be
refactored into p2 and vice versa`.

We've already seen a couple when discussing transforming outputs:
```scala
// 1. Transforming with a no-op is the same as not transforming
p.map { x => x } <-> p
// 2. We can fuse two transformations into one
p.map(f).map(g) <-> p.map(f.andThen(g))

// Refactoring example for 1. and 2.
Console
  .readLine
  .map { input => input.toUppercase }
  .map { str => str.length }
  .map { result => result }
         <->
Console.readLine.map { input => input.toUppercase.length }
```

And we'll follow a similar format for the additional laws of chaining:
providing a description in English, an equivalence with `<->`, and an
example of refactoring.

The first set of laws are variations of the same idea: if your
chaining revolves exclusively around emitting an output, you don't
actually need to chain.
```scala
// 3. Chaining to emit a transformed result is the same as transforming
p.flatMap { x => pure(f(x)) } <-> p.map { x => f(x) }
// 4. Chaining only to emit is a no-op. Follows from 3. and 1.
p.flatMap { x => pure(x) } <-> p
// 5. Emitting before chaining with a function is just a call to the function
pure(a).flatMap { x => f(x) } <-> f(a)

// Refactoring example for 3.
Console
  .readLine
  .flatMap { line => Console.pure(line.length) }
         <->
Console
  .readLine
  .map { line => line.length }

// Refactoring example for 4.
Console.readLine.flatMap { line => Console.pure(line) }
         <->
Console.readLine

// Refactoring example for 5.
Console
  .pure("hello")
  .flatMap { word => Console.print(word) }
         <->
Console.print("hello")
```
The final law deals with nesting:
```scala
// 6. Sequences of dependencies can be nested or unnested
p.flatMap { x =>
  f(x).flatMap { y =>
    g(y)
  }
}
         <->
p
 .flatMap { x => f(x) }
 .flatMap { y => g(y) }

// Refactoring example for 6.
def prompt(s: String): Console[String] =
  Console.print(s).flatMap { _ => Console.readLine }

prompt("What's your name?").flatMap { name =>
  prompt(s"Hello $name, what's your favourite food?").flatMap { food =>
    prompt(s"I like $food too! And where are you from?")
  }
}
         <->
prompt("What's your name?")
  .flatMap { name => prompt(s"Hello $name, what's your favourite food?") }
  .flatMap { food => prompt(s"I like $food too! And where are you from?") }
```
It can appear a bit puzzling, but it's just stating "nothing weird
happens when you nest programs", since it's the exact equivalent of
the following behaviour that we take for granted in execution as
evaluation:
```scala
def prompt(s: String): String = {
  println(s)
  scala.io.StdIn.readLine()
}

val name: String = prompt("What's your name?")
val food: String = prompt(s"Hello $name, what's your favourite food?")
prompt(s"I like $food too! And where are you from?")
        <->
val food: String = {
  val name: String = prompt("What's your name")
  prompt(s"Hello $name, what's your favourite food?")
}
prompt(s"I like $food too! And where are you from?")
```

Let's conclude with an example of using laws to refactor one of our
greeting programs:

```scala
Console
  .readLine
  .map { username => s"Hello, $username!" }
  .flatMap { greeting => Console.print(greeting) }
// law 3: transform `map` into `flatMap`
Console
  .readLine
  .flatMap { username => Console.pure(s"Hello, $username!") }
  .flatMap { greeting => Console.print(greeting) }
// law 6: nest
Console
  .readLine
  .flatMap { username =>
     Console.pure(s"Hello, $username!")
       .flatMap { greeting => Console.print(greeting) }
   }
// law 5: eliminate `pure` by applying function directly
Console
  .readLine
  .flatMap { username => Console.print(s"Hello, $username!") }
```

## Conclusion

In this post we introduced the real names of `pure`, `map` and
`flatMap` and shown how programs don't execute without `flatMap`,
before exploring the refactoring rules of chaining. Next time we'll be
looking at enriching our algebras by equipping them with _error
handling_.
