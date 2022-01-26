---
title: "Programs as Values, Part VI: Chaining"
date: 2022-01-20
---

I'm now going to introduce the `Console` algebra, an evolution of
[Out](https://systemfw.org/posts/programs-as-values-IV.html) and
[In](https://systemfw.org/posts/programs-as-values-V.html) that will
accompany us for the next few instalments of this series.

We will start from an imperfect version, here's how it looks like:

```scala
/*
 * carrier:
 *   Console[A]
 *     where A represents the output of a Console program
 * introduction forms:
 *   readLine: Console[String]
 *   print: String => Console[Unit]
 * combinators:
 *   andThen[A]: (Console[A], Console[A]) => Console[A]
 *   transformOutput[A, B]: (Console[A], A => B) => Console[B]
 * elimination forms:
 *   run[A]: Console[A] => IO[A]
 */
 sealed trait Console[A] {
   def andThen(next: Console[A]): Console[A]
   def transformOutput[B](transform: A => B): Console[B]

   def run: IO[A]
   ...
 object Console {
   val readLine: Console[String]
   def print(s: String): Console[Unit]
   ...
```
`readLine` and `transformOutput` have a familiar shape, but `print`
and `andThen` need to fit the `Console[A]` shape, so we use the `Unit`
type to express that printing has no meaningful output:

```scala
def print(s: String): Console[Unit]
```

and parameterise `andThen` with `A` everywhere:

```scala
// andThen[A]: (Console[A], Console[A]) => Console[A]

sealed trait Console[A] {
   def andThen(next: Console[A]): Console[A]
   ...
```

and we can write `Console` programs!

```scala
val helloWorld: Console[Unit] =
  Console.print("Hello ").andThen(Console.print("World!"))

val lineLength: Console[Int] =
  Console.readLine.transformOutput(line => line.length)
```

Obviously to actually execute these programs, they have to be
converted to `IO` via `run` and then embedded into an `IOApp`, but we
will ignore the elimination form for the remainder of the article, and
focus on writing programs with `Console`.

## A sample program

We will explore and evolve the `Console` algebra whilst
trying to write the following program:

- Ask the user to enter their username.
- Read it from stdin.
- Create a greeting message like `"Hello, $username!"`.
- Print the message to stdout.
- Extra: if the username at point 2 is empty, ask again.

We will do it in pieces, starting from a simple prompt that doesn't
handle empty usernames:

```scala
val namePrompt: Console[String] =
  Console
    .print("What's your username? ")
    .andThen(Console.readLine)

type mismatch;
[error]  found   : Console[String]
[error]  required: Console[Unit]
[error]       .andThen(Console.readLine)
[error]                        ^
```

Uh-oh, it doesn't compile: `andThen` wants both arguments to be
`Console` programs with the same type of output, but `print(s)` and
`readLine` have different output types, respectively `Console[Unit]`
and `Console[String]`.

This limitation doesn't seem reasonable, so let's relax the type of
`andThen` to allow the second program to have a different output type,
which will also be the output type of the overall expression:

```scala
// andThen[A, B]: (Console[A], Console[B]) => Console[B]

sealed trait Console[A] {
  def andThen[B](next: Console[B]): Console[B]
  ...
```
and we can write `namePrompt` unchanged:

```scala
val namePrompt: Console[String] =
  Console
    .print("What's your username? ")
    .andThen(Console.readLine)
```

Next step is to create the greeting message, which sounds like a job for
`transformOutput`:

```scala
val promptWithGreeting: Console[String] =
  Console
    .print("What's your username? ")
    .andThen(Console.readLine)
    .transformOutput { username => s"Hello, $username!" }
```

Ok we're getting there, all that's left to do now is to print the
greeting message to stdout. And here we stumble onto an interesting
problem.

## Chaining

The program we need to write has to print something we've previously
read (and transformed). In slightly more general terms, it needs to
use the _output_ of our `promptWithGreeting` program to build another
program, the program that prints that output.

In execution as evaluation, this idea is expressed by actually running
an action and naming its result:

```scala
val line: String = scala.io.StdIn.readLine()
println(line)
```

but as usual, we want to compose programs instead.

What we need is a change in perspective: whenever we need the output
of a program `p1: Console[A]` to build another program `p2:
Console[B]`, that means that `p2` _depends_ on the output of `p1`.

We've seen that the output of a `Console[A]` program is represented by
its output type parameter `A`, and the idea that `Y` depends on `X` is
expressed by a function `X => Y`, so the concept that `p2: Console[B]`
depends on the output of `p1: Console[A]` can be written as `A =>
Console[B]`.

And therein lies our problem, the only combinator that can connect two
`Console` programs is `andThen`, and we can see from its type that
there is no dependency between the two programs it takes as input:

```scala
// andThen[A, B]: (Console[A], Console[B]) => Console[B]

sealed trait Console[A] {
  def andThen[B](next: Console[B]): Console[B]
  ...
```

Let's instead introduce a new `chain` combinator which takes
dependency into account. It will take a `Console[A]` program, and a
function that uses the output of that program to decide what the next
program should be:


```scala
// chain[A, B]: (Console[A], A => Console[B]) => Console[B]

sealed trait Console[A] {
  def chain[B](next: A => Console[B]): Console[B]
  ...
```

Equipped with `chain`, we can now easily print something we've read:

```scala
val echo: Console[Unit] =
  Console.readLine.chain { line =>
    Console.print(line)
  }

// Same, but with explicit annotations for every type:
val echo: Console[Unit] =
  // chain: (Console[String], String => Console[Unit]) => Console[Unit]
  (Console.readLine: Console[String]).chain {
     (
       (line: String) =>
          Console.print(line): Console[Unit]
     ): String => Console[Unit]
  }: Console[Unit]

```

and indeed express our target program:

```scala
val promptAndGreet: Console[Unit] =
  Console
    .print("What's your username? ")
    .chain(_ => Console.readLine)
    .transformOutput { username => s"Hello, $username!" }
    .chain { greeting => Console.print(greeting) }
```

Note that in `promptAndGreet` we've replaced `print.andThen(readLine)`
with `print.chain(_ => readLine)`, i.e. a special case of `chain`
where the shape of the next program doesn't depend on the output of
the previous one, and can ignore it.

The ability to depend on the output of another computation has clearly
gained us some power, but just how much power exactly? As it turns
out, a huge amount: `next: A => Console[B]` can use `A` in _arbitrary_
ways to decide what the next computation should be. In `nameAndGreet`
we simply passed it through, but `next` could include `if/else`
expressions, recursion, pattern matching, and so on. In other words,
`chain` allows _general control flow_.


## Emitting outputs

Here's how our sample program looks like so far, with some
minimal refactoring:

```scala
val namePrompt: Console[String] =
  Console
    .print("What's your username? ")
    .chain(_ => Console.readLine)

val promptAndGreet: Console[Unit] =
  namePrompt
    .transformOutput { username => s"Hello, $username!" }
    .chain { greeting => Console.print(greeting) }
```

The only piece left is asking for a username again if the user inserts
an empty one. We could go and modify `namePrompt` accordingly, but
when you think about it there isn't much about this logic that is
actually specific to `namePrompt`: we simply want to repeat a `p:
Console[String]` until its output is non empty.

We're in programs as values, so programs that manipulate other
programs are our bread and butter:

```scala
def repeatOnEmpty(p: Console[String]): Console[String] = ???
```

So how do we implement `repeatOnEmpty`? We need to use the string
outputted by `p` to make a decision based on whether it's empty or
not, which is to say we need `chain`:

```scala
def repeatOnEmpty(p: Console[String]): Console[String] =
  p.chain { str =>
    if (str.isEmpty) ???
    else ???
  }
```

if the string is indeed empty, we simply repeat the whole process
using recursion:

```scala
def repeatOnEmpty(p: Console[String]): Console[String] =
  p.chain { str =>
    if (str.isEmpty) repeatOnEmpty(p)
    else ???
  }
```

and if it's non empty, that's the output of our `Console` program:

```scala
def repeatOnEmpty(p: Console[String]): Console[String] =
  p.chain { str =>
    if (str.isEmpty) repeatOnEmpty(p)
    else str
  }

type mismatch;
[error]  found   : String
[error]  required: Console[String]
[error]       else str
[error]            ^
```

Oh, another compile error... both branches of an `if/else` need to
have the same type, whereas in our case the `if` branch has type
`Console[String]`, and the `else` branch has type `String`.

On second thought, it doesn't make sense for `repeatOnEmpty` to return
a `String`: `repeatOnEmpty` needs to return a _program_, i.e. an
instance of a datatype that represents commands that will eventually
be executed, and a `String` is not the same thing as a command to emit
one. This means that our `Console` algebra is missing an introduction
form, the ability to emit an output:

```scala
// emitOutput[A]: A => Console[A]

object Console {
  def emitOutput[A](a: A): Console[A]
  ...
```

and there we have it:

```scala
def repeatOnEmpty(p: Console[String]): Console[String] =
  p.chain { str =>
    if (str.isEmpty) repeatOnEmpty(p)
    else Console.emitOutput(str)
  }
```

## The final program

Here's what the final version of `Console` looks like:

```scala
/*
 * carrier:
 *   Console[A]
 *     where A represents the output of a Console program
 * introduction forms:
 *   readLine: Console[String]
 *   print: String => Console[Unit]
 *   emitOutput[A]: A => Console[A]
 * combinators:
 *   chain[A, B]: (Console[A], A => Console[B]) => Console[B]
 *   transformOutput[A, B]: (Console[A], A => B) => Console[B]
 * elimination forms:
 *   run[A]: Console[A] => IO[A]
 */
 sealed trait Console[A] {
   def chain[B](next: A => Console[B]): Console[B]
   def transformOutput[B](transform: A => B): Console[B]

   def run: IO[A]
   ...
 object Console {
   val readLine: Console[String]
   def print(s: String): Console[Unit]
   def emitOutput[A](out: A): Console[A]
   ...
```

and here's our final program:

```scala
def repeatOnEmpty(p: Console[String]): Console[String] =
  p.chain { str =>
    if (str.isEmpty) repeatOnEmpty(p)
    else Console.emitOutput(str)
  }

val namePrompt: Console[String] =
  Console
    .print("What's your username? ")
    .chain(_ => Console.readLine)

val promptAndGreet: Console[Unit] =
  repeatOnEmpty(namePrompt)
    .transformOutput { username => s"Hello, $username!" }
    .chain { greeting => Console.print(greeting) }
```

#### Note on conciseness
You might be thinking that our program above is rather
verbose compared to just:

```scala
 def p: Unit = {
   var user: String = ""
   while (user.isEmpty) {
     println("What's your username?")
     user = scala.io.StdIn.readLine()
   }
   println(s"Hello, $user!")
 }
```
but remember that I'm really spelling things out in the examples.
Here's how it looks like in real code, using `cats.effect.IO` and
combinators defined in the [cats](github.com/typelevel/cats) library:
```scala
  val p: IO[Unit] =
    (IO.println("What's your name?") >> IO.readLine)
      .iterateWhile(_.isEmpty)
      .flatMap { user => IO.println(s"Hello, $user!") }
 ```

## Conclusion

In this article we saw the essential concept of _chaining_: creating
programs that can depend on the output of a previous program.
Chaining represents a big leap in the expressiveness of our algebras,
as we are now able to express _arbitrary sequential control flow_.

Next time we will explore some of properties of `chain` and
`emitOutput`, as well as introducing the real names used in
[cats](github.com/typelevel/cats) and
[cats-effect](github.com/typelevel/cats-effect), as we make our way
towards writing real code in programs as values.

---

## Appendix: implementation

This series puts a big stress on _algebraic thinking_: reasoning on
programs as values datatypes using the operations defined on them
rather than their internal structure. This is a powerful approach
because it scales from simple datatypes like `Option`, to datatypes
like `IO` whose internal structure and implementation is extremely
advanced.

However, there is a risk that you might think that "command" datatypes
like `Console` are utterly magical, so I'm going to make an exception
and show you an implementation for it:

```scala
sealed trait Console[A] {
  def chain[B](next: A => Console[B]): Console[B] =
    Console.Chain(this, next)

  def transformOutput[B](transform: A => B): Console[B] =
    // curious about this? We'll talk about it next time!
    chain { output =>
      Console.emitOutput(transform(output))
    }

  def run: IO[A] =
    Console.translateToIO(this)
}
object Console {
  def readLine: Console[String] =
    ReadLine

  def print(s: String): Console[Unit] =
    Print(s)

  def emitOutput[A](a: A): Console[A] =
    EmitOutput(a)

  case object ReadLine extends Console[String]
  case class Print(s: String) extends Console[Unit]
  case class EmitOutput[A](a: A) extends Console[A]
  case class Chain[AA, A](fa: Console[AA], f: AA => Console[A])
      extends Console[A]

  def translateToIO[A](c: Console[A]): IO[A] = c match {
    case Console.ReadLine => IO.readLine
    case Console.Print(s) => IO.println(s)
    case Console.EmitOutput(a) => IO.pure(a)
    case Console.Chain(fa, f) =>
      translateToIO(fa).flatMap(x => translateToIO(f(x)))
  }
}
```

As you can see, we really do mean programs are _values_: `Console` is
literally a datatype, which then gets translated to `IO`, which is
another datatype. All the execution happens in the layer that
interprets `IO` into actual side effects when the JVM calls `main`, as
we will see once our series gets to discussing `IO`.
