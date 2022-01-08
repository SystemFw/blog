---
title: "Programs as Values, Part V: Outputs"
date: 2022-01-04
---

We want to write an algebra that reads from stdin, and use it to write
the following program:

> read a `String`, compute its length, and return `true` if the length
> is greater than 10, or `false` otherwise.

To represent the action of reading a line from stdin, we could use a
simple algebra such as this one:

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

but obviously that's not enough to write our program: we need to do
some extra transformations on the line we've read. 

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
val out: Boolean = readLine.run.length > 10
```

but this is not a fruitful direction: we are basically saying that the
only way to change the output of a program written in the `In` algebra
is to eliminate the algebra entirely.

Algebras are our unit of composition, therefore with the elimination
approach any program that needs to change its output can no longer be
composed, which is a _really_ strong limitation: for example in [Part
III](https://systemfw.org/posts/programs-as-values-III.html) we saw
that for `IO`, elimination happens when the JVM calls `main`, it would
be really weird if we couldn't encode something as simple as
`String.length` until then.

Instead, we want to have the ability to transform outputs without
leaving our algebras, and therefore we have to enrich it with a
_combinator_. Recall that the general shape of a combinator is:

```scala
changeOutput: (In, ...) => In
```

and we need to fill the `...` with something that can encode the
concept of transforming one thing into another. Well, we already have
a well known concept for this: functions. So, `changeOutput` needs to
take a function, but we have a problem: what type should this function
be, in order to fit the possible transformations we want to encode
such as `_.length` or `_ > 10` ?

Of course, an `Any => Any` fits anything:

```scala
/*
 * carrier:
 *   In
 * introduction forms:
 *   readLine: In
 * combinators:
 *   changeOutput: (In, Any => Any) => String
 */
sealed trait In {
  def changeOutput(transform: Any => Any): In
  ...
object In {
  val readLine: In
  ...
```

but this is also not an acceptable solution: our algebra has gained
power, but we have lost type safety altogether.


What we can do for now is passing an `` function that works e
```scala
changeOutput: (In, Any => Any) => In
```



you might try with an elim form, but it's not fruitful, the program is about changing the output (where do I put this remark?), and algebra is our unit of composition, so what I'd be saying is that every time I want to change an output I can no longer compose, we will see this point in greater depth once we talk about `cats.effect.IO` and `cats.effect.Resource`


at some point I should say: no elimination forms in this article
`In` algebra: read a String, count it's length, return false/true whether it's greater than 10
introduce the necessity for type params

Console algebra, program 1
You might have noticed that we wrote console from scratch, rather than attempting to compose Out and In. There are techniques to achieve such a modular composition of effects, but they are out of scope for now.
write prompt, then read, convert to upper case

program 2: monads

talk about laws a bit:


<!-- ------- -->
<!-- possible plan:  -->
<!-- results, V  -->
<!-- errors, VI -->
<!-- iteration, VII -->
<!-- combinator deluge VIII --> <!-- maybe swap iteration with deluge? -->
<!-- IO & FFI IX -->
<!-- basic concurrency X  -->
<!-- resource XI  -->
<!-- effectful constructors XII -->
<!-- advanced concurrency/state -\-> should this be a separate series? XIII -->
<!-- abstraction? should this be a separate series? -->
<!-- ------- -->


<!-- -------- -->
<!-- errors -->
<!-- -------- -->
<!-- Short Circuit algebra: option -->
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
<!-- combinator deluge VIII -->
<!-- slightly different, we'll show vocab of combinators we have gained -->
<!-- f-a-m, monaderror, use Console version with errors -->
<!-- example, combinator, raw form -->


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
recall interruption from parMapN
intro form make
elim form use
show flatMap
show lifting with eval (print during resource acquisition)
add an example restating referential transparency at the end, to introduce effectful constructors
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
