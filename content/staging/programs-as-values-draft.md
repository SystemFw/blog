---
title: "Programs as Values, Part V: Outputs"
date: 2022-01-04
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
    .transformOutput(input => input.length)
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
talk about IO in more detail.

## Laws

You might be wondering why I have written the final program as:

```scala
val prog1: In[Boolean] =
  readLine
    .transformOutput(input => input.length)
    .transformOutput(length => length > 10)
```

as opposed to:

```scala
val prog2: In[Boolean] =
  readLine.transformOutput(input => input.length > 10)
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

with
  p: In[A]
  f: A => B
  g: B => C
```

which means that we can switch between `prog1` and `prog2` at will,
and not just in the case where `p = readLine`, `f = _.length`, and `g
= _ > 10`, but for _any_ `p`, `f`, and `g`, as long they have the
correct type. So in this case, we can use laws as a _refactoring
aid_ : they gave us freedom to refactor by specifying which
transformations on our programs are harmless.

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

with
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


## Chaining

I'm now going to introduce the algebra that will accompany us for the next few instalments of this series, the `Console` algebra. We will start with an imperfect version, and iterate:

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

as mentioned above, we will ignore the `run` elimination form for the
remainder of the article , and focus on writing programs with
`Console`.

You might have noticed that we wrote console from scratch, rather than attempting to compose Out and In. There are techniques to achieve such a modular composition of effects, but they are out of scope for now.


talk about Console[Unit]
talk about andThen, just adding the type param

show how andthen and map are differnet with print. Make point about laws as _understanding aid_.

Console algebra, program 0
read, convert to uppercase

Console algebra, program 1
write prompt, then read, convert to upper case

program 2: write prompt, then read, convert to upper case, print upper case
try with andThen, and show compile error
then with transformOutput, and expand a bit on why nothing happens (annoying detour to show the printing)
andThen gives a starting point, recall from part III
introduce chain
introduce chainNested





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
