---
title: "Programs as Values, Part III: Explicit Control Flow"
date: 2021-05-03
---

At the end of [Part
II](https://systemfw.org/posts/programs-as-values-II.html), we arrived
at a description of the programs as values paradigm: we represent
effects as datatypes (i.e. programs as values), and we use functions
to assemble big programs out of smaller ones, until the last
instruction in `main` translates them to actual side effects.

Before diving more deeply in the structure of these datatypes, I'd
like to expand on the two fundamental ideas:

- We can represent execution through explicit combinators. We will see
  how this is possible even in the execution as evaluation paradigm.
- Programs as values datatypes really do behave as values. We will
  show this with very simple examples based on `cats.effect.IO`.


## Making execution explicit

In this section we will look at how to express execution via explicit
combinators. We don't yet know how to do it in programs as values, so
we are going to do it in execution as evaluation.
In particular we are looking to express the following program:

```scala
def p = {
  println("What's your name?")
  Thread.sleep(1000)
  val yourName = scala.io.StdIn.readLine()
  println(s"Hello, $yourName!")
  yourName.length
}
```

This type of sequential control flow is very natural in
execution as evaluation, but on deeper thought it relies on two
"magical" features: one is that a newline/semicolon between two
instructions `A` and `B` means "execute `A` before `B`", and the other
is that we can refer to the result of an instruction in subsequent
instructions by binding it to a `val`.

So here's a challenge: in a strict language, is it possible to express
`p` without relying on the two magical features I just described?
Remember that by _strict_ language we mean a language where the
evaluation semantics are `call-by-value`, which means (informally)
that the arguments of a function are evaluated left to right before
the body of that function.

Actually step away from the page and spend a few minutes thinking how
we could do this.

Did you manage? Nice!, but don't worry if you didn't: it's
counterintuitive at first, but we can get there by looking at
`call-by-value` in more detail.

Let's take this example:

```scala
def two = 2
def three = 3
def add(a: Int, b: Int): Int = a + b

add(two, three)
```

and add `println`s to trace evaluation:

```scala
def two = {
 println("Evaluating 2")
 2
}

def three = {
 println("Evaluating 3")
 3
}

def add(a: Int, b: Int): Int = {
  println("Evaluating addition")
  a + b
}

add(two, three)
// Evaluating 2
// Evaluating 3
// Evaluating addition
// res1: Int = 5
```

The execution trace closely resembles sequential control flow, which
gives us an idea on how we can sequence two instructions by exploiting
the evaluation order of the arguments of a function:

```scala
def seq1[A, B](fst: A, snd: B): Unit = ()

seq1(
  println("first"),
  seq1(
    println("second"),
    println("third")
  )
)
// first
// second
// third
```

But that's not general enough: when using the normal
newline/semicolon, the last expression becomes the overall result. So
we can slightly modify our `seq1` to return the second argument
instead:

```scala
def seq2[A, B](a: A, b: B): B = b

seq2(
  println("one"),
  42
)
// one
// res3: Int = 42
```

We are getting there, but we still cannot express the second
"magical" feature, the ability to refer to the result of an
instruction in subsequent instructions. This program:

```scala
val s = scala.io.StdIn.readLine()
s.length + 1
```
would translate to:

```scala
seq2(
  scala.io.StdIn.readLine(),
  s.length + 1
)
// error: not found: value s
//   s.length + 1
//   ^
```

To get to the final solution, we need to focus on the role of `val
yourName` in this snippet:

```scala
 val yourName = scala.io.StdIn.readLine()
 println(s"Hello, $yourName!")
 yourName.length
```

`val yourName` conceptually divides the program in two halves: the
first half computes `yourName`, and the second half _depends_ on it.

`seq2` cannot represent this idea because the two halves of the
program are represented as `A` and `B`, and there is no relationship
between them:

```scala
def seq2[A, B](a: A, b: B): B = b
```

but that shows us the path to a solution, we need to express the idea
that `B` _depends on_ `A`, i.e. that `B` is a function of `A`:

```scala
def seq3[A, B](a: A)(f: A => B): B = f(a)
```

and we can now write:

```scala
seq3(scala.io.StdIn.readLine())(s => s.length + 1)
```

`seq3` also works for the simple sequencing case, by ignoring its argument:

```scala
seq3(println("hello"))(_ => println("world"))
```

which makes sense, normal semicolons can also be seen as a special
case of named results, i.e.

```scala
println("hello")
println("world")
```

can be seen as:

```scala
val _ = println("hello")
val _ = println("world")
```

**We've solved our problem**, and can now express the original program:

```scala
def seq[A, B](a: A)(f: A => B): B = f(a)

def original = {
  println("What's your name?")
  Thread.sleep(1000)
  val yourName = scala.io.StdIn.readLine()
  println(s"Hello, $yourName!")
  yourName.length
}

def explicit =
  seq(println("What's your name?")) { _ =>
    seq(Thread.sleep(1000)) { _ =>
      seq(scala.io.StdIn.readLine()) { yourName =>
        seq(println(s"Hello, $yourName!")) { _ =>
          yourName.length
        }
      }
    }
  }
```

So, why did we do this? The implementation of `seq` still relies on
evaluation, and the effectful building blocks like `println` aren't
represented as datatypes/values, but `explicit` shows that we can
describe execution through combinators, and that _structure_ can be
applied to things which _are_ values, like `cats.effect.IO`:

```scala
import cats.effect.IO
import scala.concurrent.duration._

 // works entirely differently from `seq`,
 // but expresses the same idea
def seqIO[A, B](a: IO[A])(f: A => IO[B]): IO[B] =
  a.flatMap(f)

def p2 =
  seqIO(IO.println("What's your name")) { _ =>
    seqIO(IO.sleep(1.second)) { _ =>
      seqIO(IO.readLine) { yourName =>
        seqIO(IO.println(s"Hello, $yourName!")) { _ =>
          IO.pure(yourName.length)
        }
      }
    }
  }

// or, idiomatically:

IO.println("What's your name") >>
IO.sleep(1.second) >>
IO.readLine.flatMap { yourName =>
  IO.println(s"Hello, $yourName!").as(yourName.length)
}
```


## IO is a value

In programs as values effects are represented as datatypes, and `IO`
is the most fundamental datatype in the paradigm: an `IO[A]`
represents a program that can perform arbitrary effects, and produce a
result of type `A`.

In the previous section I said that `cats.effect.IO` is a value, so
what does this mean?

Let's take a look at this program:

```scala
import cats.effect.IO

val a: IO[Unit] = IO.println("hello")
// a: IO[Unit] = IO(...)
```

As you can see, evaluating `a` doesn't print anything, it literally
just returns an instance of the `IO` datatype. In fact, it isn't any
different from:

```scala
val b: Int = 42
// b: Int = 42
```

`IO` gets translated into actual side-effects only in `main`, and
cats-effect defines an `IOApp` trait as the entry point of your
application:

```scala
import cats.effect.IOApp

object MyApp extends IOApp.Simple {
  def run: IO[Unit] = a
}
```

The translation happens when the JVM calls `main`:

```scala
MyApp.main(Array())

// hello
```


Because `IO`s are values, relying on evaluation to sequence two `IO`s
has no effect:

```scala
val a: IO[Unit] = {
  IO.println("hey")
  IO.println("hello")
}
```

```scala
MyApp.main(Array())

// hello
```

Which is very surprising at first, but it's equivalent to saying that
the number `34` is discarded in:

```scala
val c = {
  34
  "something"
}
// c: String = "something"
```

Both `IO.println("hey")` and `34` are just values, and have no effect
if you do nothing with them.

Sequencing has to be explicit through combinators, just like we saw in
the previous section:

```scala
val a: IO[Unit] =
  IO.println("hey").flatMap { _ =>
    IO.println("hello")
  }

// can be shortened to
 IO.println("hey") >> IO.println("hello")
```

```scala
MyApp.main(Array())

// hey
// hello
```

And above all, remember that values are _referentially transparent_,
so the two programs below describe the same behaviour, which is to
print twice:

```scala
IO.println("hello") >> IO.println("hello")
```
```scala
val p = IO.println("hello")
p >> p
```

just like the two expressions below describe the same result:

```
2 + 2
```
```
val x = 2
x + x
```

_You can always give a name to a value, and you can always replace a
name with its value_.

## Explicit control flow with values

In this post we saw that using datatypes to describe effects requires
modelling execution with explicit combinators, and that we can readily
emulate sequential flow with this approach.

In most execution as evaluation languages, enriching control flow
beyond that requires additional features, such as `try/catch`, `for`
loops, or `async` functions, but those concepts can all be represented
as combinators over values:

```scala
ioA.handleErrorWith(ioB)
listA.traverse(i => myIO(i))
(ioA, ioB).parMapN(f)
```

and the same approach scales all the way to very rich, compositional apis:

```scala
val randomWait =
  Stream
    .eval(Random.scalaUtilRandom[IO])
    .flatMap { random => Stream.repeatEval(random.nextInt) }
    .evalMap { n => IO.sleep(n.nanos) }

val hello = IO.println("hello")

Stream
  .repeatEval(hello)
  .zipLeft(randomWait)
  .take(10)
  .interruptAfter(10.seconds)
```

as well as to other kinds of effectful programs, such as parsers:

```scala
val listSep: Parser[Unit] =
  Parser.char(',').surroundedBy(whitespace.rep.void).void
```

Next time, we are going to start looking in detail at the structure of
programs as values datatypes, by talking about _algebras_. See you
then!

<!-- algebraic structure: intro form, combinators, elimination forms -->

<!-- Algebras comes from universal algebra -->
<!-- An algebra is a set (called the carrier) with a finite set of total functions that have the carrier set as their common codomain. -->

<!-- A Sigma algebra (signature algebra) defines the set of typed operator symbols without specifyingfunctions that would be the actual operators. Thus a signature defines a class of algebras, those whose operators conform to the typing contraints (used for typeclasses, ML modules, abstract data type etc) -->




<!-- progression: monoid, why F[A], functor, (split here?) monad, -->
<!-- monaderror -->



<!-- --- -->

<!-- programs as values notes -->
<!--   algebra -->
<!--   type A -- carrier -->
<!--   intro: SomeOtherThanAType => A, primitives: A (intro form of shape () => A) -->
<!--   combinators: (something) -> A -> A -->
<!--   elimination forms: A => (something else) => SomeOtherThanAType -->
<!--   -- write strings to "stdout" -->
<!--   type Put -->
<!--   def string(s: String): Put // String => Put, intro -->
<!--   def plus(a: Put, b: Put): Put // combinator -->
<!--   ----- -->
<!--   def run: Put => List[String] // elimination form -->

<!--   val a: Put = string("hello") -->

<!--   val helloWorld: Put = a.plus(string("world")) -->

<!--   def writeN(in: Put, n: Int): Put = -->
<!--     n match { -->
<!--       0 => string("") -->
<!--       n => in.plus(writeN(in))plus(writeN(in, n - 1), in) -->
<!--     } -->
<!--   ----------- -->
<!--   List[Put] -->
<!--   ----------- -->
<!--   type Console[A] <--  output  /// (Put + Read) -->
<!--         ^^ language -->
<!--   ----- intro -->
<!--   def read: Console[String] -->
<!--   def put(s: String): Console[Unit] -->
<!--   def pure[A](s: A): Console[A] // lift, with no effect -->
<!--   ----- combinators -->
<!--   Console[A] => (A => B) => Console[B] -->
<!--   (A => B) => (Console[A] => Console[B]), lifting A => B into the Console language -->
<!--   def transformOutput[A, B](p: Console[A],trasformation: A => B): Console[B] = ??? -->
<!--       map -->
<!--   def andThen(p: Console[A], transformation: A => Console[B]): Console[B] = -->
<!--       flatMap/chaining -->
<!--   def flatten: Console[Console[A]] => Console[A] -->
<!--   ----- elim <-- forget -->

<!--   ++,empty Monoid -->
<!--   map Functor -->
<!--    (A => B) => F[A] => F[B] -->
<!--     (A => B => ... => N) => F[A] => F[B] => ... => F[N] -->
<!--     flatMap -->
<!--   andThen, pure Monad -->

<!--   andThen -\-> sequential, arbitrary control flow -->

<!--   read a String, count the length, print that -->
<!--   read, put, tOut -->

<!--   val p: Console[Unit] = read // Console[String] -->
<!--     .transformOutput(_.length) // Console[Int] -->
<!--     .transformOutput(_.toString) // Console[String] -->
<!--     .andThen(put) -->
<!--   def repeatN(p: Console[A], n: Int): Console[A] = -->
<!--     n match { -->

<!--     } -->
<!--   Stream[F[_], A] <-- type, Stream[IO, A] -->
<!--   ----- intro -->
<!--   empty: () => Stream[IO, A] -->
<!--   fromList: List[A] => Stream[IO, A] -->
<!--   eval(action: IO[A]): Stream[IO, A] -->
<!--   ---- tons -->
<!--   flatMap: Stream[F, A] => (A => Stream[F, B]) => Stream[F, B] -->
<!--   ++: Stream[F, A] => STream[F, A] => Stream[F, A] -->
<!--   take: Int => Stream[F, A] => Stream[F, A] -->
<!--   concurrently: Stream[F, A] => Stream[F, Unit] => Stream[F, A] -->
<!--    .... -->
<!--    ---- -->
<!--   compile.drain: Stream[F, A] => IO[Unit] -->
<!--   compile.list: Stream[F, A] => IO[List[A]] -->
<!--   compile.fold -->

<!-- } -->
