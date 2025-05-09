---
title: "Programs as Values, Part VII: Exploring Chaining"
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
  .flatMap { line =>
    val lineLength = line.length
    Console.pure(lineLength)
  }
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

val name: String = prompt("What's your name")
val location: String = {
  val food: String = prompt(s"Hello $name, what's your favourite food?")
  prompt(s"I like $food too! And where are you from?")
}
         <->
val name: String = prompt("What's your name?")
val food: String = prompt(s"Hello $name, what's your favourite food?")
val location: String = prompt(s"I like $food too! And where are you from?")
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


<!-- ------- -->
<!-- possible plan:  -->
<!-- outputs, V  -->
<!-- outputs, VI  -->
<!-- errors, VII -->
<!-- combinators everywhere VIII -->
<!-- effectful iteration IX -->
<!-- IO & FFI X -->
<!-- basic concurrency XI  -->
<!-- resource XII  -->
<!-- effectful constructors XIII -->
<!-- advanced concurrency/state -\-> should this be a separate series? -->
<!-- abstraction? should this be a separate series? -->
<!-- ------- -->


<!-- -------- -->
<!-- errors -->
<!-- -------- -->
<!-- Short Circuit algebra: option -->
<!-- note on laws as a _specification aid_ -->
<!-- Errors algebra: Either -->
<!-- Do I use two type params for Errors? or more simply show <-> Either[Throwable, A]
<!-- add MonadThrow to Console -->
<!-- Make a point that we aren't making any claims about interaction with try/catch here (it will be a point for the FFI) -->
<!-- -------- -->

<!-- -------- -->
<!-- iteration -->
<!-- -------- -->
<!-- introduce List as abstract, intro form, some combinators (map, -->
<!-- flatMap, zip, takeWhile), foldRight. Try doing `List(Questions) => -->
<!-- Console[List[Outputs]]`, show the shape of `traverse` on `List`, -->
<!-- possibly `sequence` as well, then say we generally don't look at -->
<!-- implementations, but look no magic: foldRight + `pure/mapN`. Then show -->
<!-- the same for Option. Make a small point about "You might be wondering -->
<!-- if these can be abstracted, much the same as with the combinators on -->
<!-- touch on abstraction -->
<!-- F-A-M, yes but out of scope for now" > -->
<!-- -------- -->

<!-- -------- -->
<!-- combinator deluge -->
<!-- -------- -->
<!-- better titles: combinators abound, combinators everywhere -->
<!-- combinator deluge VIII -->
<!-- slightly different, we'll show vocab of combinators we have gained -->
<!-- f-a-m, monaderror, use Console version with errors -->
<!-- example, combinator, raw form

One of the principles behind programs as values is that you can
represent complex logic easily by building programs that manipulate
other programs in the form of combinators. Today we will substantiate
this claim by showing that the building blocks we've seen so far, i.e.
`map`, `flatMap`, `pure`, `raiseError` and `handleErrorWith` can be
used to derive a _large_ variety of combinators that abstract a wealth
of common control flow patterns.

Even better, those combinators have already been written for you by
the [cats](https://github.com/typelevel/cats) library: you tell `cats`
that your algebra includes the required functions, and you get all the
derived combinators for free. The mechanism used to achieve this -
*typeclasses* - is interesting but out of scope for now, so we will
ignore its details. The one important takeway when using datatypes in
the cats ecosystem like `cats.effect.IO`, `cats.effect.Resource`,
`fs2.Stream` or `cats.parse.Parser` is the following:

> Always remember to add
> ```
> import cats.syntax.all._
> ```
> to your toplevel import list.


This article is different from the others in the series in that you're
not required to read it top to bottom, you can keep it as a reference
and come back to it whenever you want. We will present several
combinators, and for each of them give a translation into our basic
operation, and an example. I've tried to put the most common
combinators towards the top.
-->


<!-- -------- -->
<!-- basic concurrency -->
<!-- -------- -->
<!-- parMapN, race, sleep, timeout (possibly parTraverse)? -->
<!-- need start/cancel here? to show background, -->
<!-- or do I use the resource article for it? -->
<!-- -------- -->

<!-- -------- -->
<!-- resource -->
<!-- -------- -->
<!-- recall interruption from parMapN -->
<!-- intro form make -->
<!-- elim form use -->
<!-- show flatMap -->
<!-- show lifting with eval (print during resource acquisition) -->
<!-- add an example restating referential transparency at the end, to introduce effectful constructors -->
<!-- -------- -->


<!-- ---------- -->
<!--  effectful constructors -->
<!-- ---------- -->
<!-- trait A { -->
<!--  def foo: B -->
<!-- } -->
<!-- point about how to name these abstractions? remark the point about algebras being overloaded. Probably going to call them interfaces -->
<!-- remark that trait != sealed trait -->
<!-- class MyA(...) extends A -->
<!-- def myA(...): A = new A { -->
<!-- } -->
<!-- version with IO: -->
<!-- then show constructors with effects, and resource -->
<!-- stateful constructors (explain allocation is mutable? or just that you will receive it, maybe add an appendix) -->
<!-- constructors that initialise a resource -->
<!-- resource that spawn concurrent processes -->
<!-- constructors that reuse other constructors that are effectful -->
<!-- regions of sharing -->
<!-- maybe introduce very basic Ref here instead, use an example with concurrency, like mapN with additions to a counter, well, maybe we need to do it before regions of sharing -->
<!-- appendix: something that wraps `var` to show that allocation needs to be wrapped: show the two constructors, which one is the right one? -->
<!-- --------------- -->



<!-- ------- -->
<!-- sources for algebras -->
<!-- https://okmij.org/ftp/tagless-final/Algebra.html -->
<!-- https://books.google.it/books?id=MS2f1AATHIoC&pg=PA267&lpg=PA267&dq=with+a+finite+set+of+total+functions+that+have+the+carrier+set+as+their+common+codomain.&source=bl&ots=rRTtRtO-hY&sig=ACfU3U1b8lOc189R8gaOSEzlFjjmXYHBKA&hl=en&sa=X&ved=2ahUKEwiOpcuh75b1AhUJM-wKHdxBBpoQ6AF6BAgREAM#v=onepage&q=with%20a%20finite%20set%20of%20total%20functions%20that%20have%20the%20carrier%20set%20as%20their%20common%20codomain.&f=false -->
<!-- https://en.wikibooks.org/wiki/Universal_Algebra/Definitions,_examples -->
<!-- ------- -->


<!-- ----- -->
<!-- Initial programs as values notes -->
<!-- progression: monoid, why F[A], functor, (split here?) monad, -->
<!-- monaderror -->
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
<!-- ------- -->

<!-- Stick this somewhere, possibly a different article -->
<!-- ## Appendix: the value of laws -->
<!-- The focus on algebraic laws can be a bit polarising. -->
<!-- Some people seem to believe that the existence of laws makes FP -->
<!-- automatically correct or better than anything else "because Maths", -->
<!-- which is obviously nonsense. Others think laws are ivory tower -->
<!-- nonsense that serves no purpose, which is unfair. -->
<!-- They are useful because: -->
<!-- - refactoring aids, part V, functor -->
<!-- - understanding aids, part VI, `F[F[A]]` has no effects -->
<!-- - specification aids, part VII, short circuiting errors -->
<!-- - semantics aids, idempotency or monoid associativity -->
<!-- In addition to that, a lot of them will seem absolutely obvious and -->
<!-- unremarkable. Under this lens, most laws are a promise from the -->
<!-- implementor of the algebra to the user, saying "I'm not doing anything -->
<!-- weird". However, Note that (very) rarely, an existing law might -->
<!-- conflict with the desired behaviour in some corner cases, resulting in -->
<!-- overall behaviour that is harder to reason about. In this case, an -->
<!-- implementor might consider breaking the law in that corner case, after -->
<!-- very carefully considering it. If the implementor is successful, users -->
<!-- won't ever notice the law breakage, and just consider the overall -->
<!-- behaviour reasonable. We will _not_ look at any such examples in this -->
<!-- series, generally they are so subtle that it would require a separate -->
<!-- article. Also note that this should be taken as a justification to -->
<!-- break laws willy nilly: in most cases where one is tempted to break a -->
<!-- law, one is wrong -->
