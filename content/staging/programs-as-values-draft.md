---
title: "Programs as Values, Part VI: Chaining"
date: 2022-01-10
---

I'm now going to introduce the algebra that will accompany us for the
next few instalments of this series, the `Console` algebra. We will
start with an imperfect version, and iterate:

```scala
/*
 * carrier:
 *   Console[A]
 * introduction forms:
 *   readLine: Console[String]
 *   print: String => Console[Unit]
 * combinators:
 *   andThen[A]: (Console[A], Console[A]) => Console[A]
 *   transformOutput[A, B]: (Console[A], A => B) => Console[B]
 * elimination forms:
 *   run[A]: Console[A] => IO[A]
 *
 */
 sealed trait Console[A] {
   def andThen(next: Console[A]): Console[A]
   def transformOutput[B](transform: A => B): Console[B]
   
   def run: IO[A]
   ...
 }
 object Console {
   val readLine: Console[String]
   def print(s: String): Console[Unit]
```

although will ignore the `run` elimination form for the remainder of
the article, and focus on writing programs with `Console`.

> Note: We're implementing `Console` from scratch even though its
functionality is a combination of
[`Out`](https://systemfw.org/posts/programs-as-values-IV.html) and
[`In`](https://systemfw.org/posts/programs-as-values-V.html). There
are techniques to compose existing effects, but they are out of scope
for now.

Note that whilst there are ways to compose existing effects, they are
out of scope for now, so we're implementing `Console` from scratch even
though its functionality is a combination of
[`Out`](https://systemfw.org/posts/programs-as-values-IV.html) and
[`In`](https://systemfw.org/posts/programs-as-values-V.html).

`readLine` and `transformOutput` should be familiar from `In`, but we
need to change `print` and `andThen` slightly from `Out`, since
`Console` has the type parameter `A ` to represent its output.
Printing has no meaningful output, so we use the `Unit type:
```scala
def print(s: String): Console[Unit]
```

`readLine` and `transformOutput` should be familiar from `In`, but
unlike `Out`, `Console` has a type parameter that represents its
output, so we need to change `print` and `andThen` slightly. We use
the `Unit` type to express that printing has no meaningful output:

```scala
def print(s: String): Console[Unit]
```

And for `andThen`, we do the simplest possible thing and just
parameterise it with `A` everywhere:

```scala
// andThen[A]: (Console[A], Console[A]) => Console[A]

sealed trait Console[A] {
   def andThen(next: Console[A]): Console[A]
   ...
```



You might have noticed that we wrote console from scratch, rather than attempting to compose Out and In. There are techniques to achieve such a modular composition of effects, but they are out of scope for now.

talk about Console[Unit]
talk about andThen, just adding the type param

program -1
print and then print

Console algebra, program 0
read, convert to uppercase

Console algebra, program 0.5
write prompt, then read
change andThen

Console algebra, program 0.5
write prompt, then read, convert to upper case

important aside here to show how andthen and map are different with print. Make point about laws as _understanding aid_. Example I can use: two prints or read >> "you started!"


program 2: write prompt, then read, convert to upper case, print upper case
try with andThen, and show compile error
then with transformOutput, and expand a bit on why nothing happens (annoying detour to show the printing)
andThen gives a starting point, recall from part III
introduce chain
introduce chainNested
need an example for pure, maybe retry?



a rose by any other name
real names, laws
rewrite examples with flatMap

appendix:
show the console ADT, but do remark we will mostly ignore the structure. Maaaybe link the fiber talk

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

