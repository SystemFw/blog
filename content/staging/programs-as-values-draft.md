---
title: "Programs as Values, Part VII: Exploring Chaining"
date: 2022-01-20
---


Last time we introduced the key concept of chaining: creating programs
that can depend on the output of a previous program, and can therefore
encode _arbitrary sequential control flow_. In this short followup we
will explore some of the properties of chaining that are relevant when
writing real programs.


Before we start, here's a reminder of how the `Console` algebra that
we have used for our sample programs looks like, excluding the `run`
elimination form and the `chain_` combinator which aren't important
for this article:

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
 * ...
 */
 sealed trait Console[A] {
   def chain[B](next: A => Console[B]): Console[B]
   def transformOutput[B](transform: A => B): Console[B]
   ...
 object Console {
   val readLine: Console[String]
   def print(s: String): Console[Unit]
   def emitOutput[A](out: A): Console[A]
   ...
```


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
output (`pure`), transforming the output of another program (`map`)
and using the output of a program to determine what the next
program should be (`flatMap`).

Here's `Console` with the standard names in place:

```scala
 sealed trait Console[A] {
   def flatMap[B](next: A => Console[B]): Console[B]
   def map[B](transform: A => B): Console[B]
   ...
 object Console {
   val readLine: Console[String]
   def print(s: String): Console[Unit]
   def pure[A](out: A): Console[A]
   ...
```

and here's our sample program that greets any user with a non-empty
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
    .flatMap(_ => Console.readLine)

val promptAndGreet: Console[Unit] =
  repeatOnEmpty(namePrompt)
    .map { username => s"Hello, $username!" }
    .flatMap { greeting => Console.print(greeting) }
```
transformOutput does this

chain does this

emitOutput does this

although these names are, in my opinion, rather nice, they aren't the ones used in cats and cats-effect
In this article we will briefly explore some of the properties of chaining, with a 



This is going to be a short article exploring some of the properties
of chaining. Although the idea behind chaining has a rich theory
behind it, all the concepts in this article have been selected because
they have a practical implication.

Remember that we're working with the Console algebra:

(TODO possibly delete chain_ if not used in this article)
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
 *   chain_[B]: (Console[A], Console[B]) => Console[B]
 *   transformOutput[A, B]: (Console[A], A => B) => Console[B]
 * elimination forms:
 *   run[A]: Console[A] => IO[A]
 */
 sealed trait Console[A] {
   def chain[B](next: A => Console[B]): Console[B]
   def chain_[B](next: Console[B]): Console[B] =
     chain(_ => next)
   def transformOutput[B](transform: A => B): Console[B]

   def run: IO[A]
   ...
 object Console {
   val readLine: Console[String]
   def print(s: String): Console[Unit]
   def emitOutput[A](out: A): Console[A]
   ...
```

I'm omitting `run` and `chain_` here cause we won't use them

and we wrote the following program with it:

(TODO possibly delete this, only keep it if used )
```scala
def repeatOnEmpty(p: Console[String]): Console[String] =
  p.chain { str =>
    if (str.isEmpty) repeatOnEmpty(p)
    else Console.emitOutput(str)
  }

val namePrompt: Console[String] =
  Console
    .print("What's your username? ")
    .chain_(Console.readLine)

val promptAndGreet: Console[Unit] =
  repeatOnEmpty(namePrompt)
    .transformOutput { username => s"Hello, $username!" }
    .chain { greeting => Console.print(greeting) }
```
(TODO delete this)
```scala
  val p: IO[Unit] =
    (IO.println("What's your name?") >> IO.readLine)
      .iterateWhile(_.isEmpty)
      .flatMap { user => IO.println(s"Hello, $user!") }
 ```


## transformOutput and chain

The first thing that's worth looking at is the relationship between `transformOutput` and `chain`. Although there is a rich theoretical background behind it, the reason we're looking at it is very practical: confusion between `chain` and `transformOutput` is one of the most common beginner mistakes when writing code in programs as values.

We will be looking at a very simple `echo` program that reads a line and prints it back, then terminates

```scala
val echo: Console[Unit] =
  Console
    .readLine
    .chain(line => Console.print(line))
```

For a start, note how the types are quite similar, but not indentical:

```scala
// chain[A, B]:           (Console[A], A => Console[B]) => Console[B]
// transformOutput[A, B]: (Console[A], A =>         B ) => Console[B]

trait Console[A] {
  def chain[B](next: A => Console[B]): Console[B]
  def transformOutput[B](transform: A => B): Console[B]
```

Note that `[B]` is a type _parameter_, and therefore not necessarily the same type in both functions. We can make this clearer by renaming it:

```scala
trait Console[A] {
  def chain[X](next: A => Console[X]): Console[X]
  def transformOutput[Y](transform: A => Y): Console[Y]

```




The `next` function




def chain[B](next: A => Console[B]): Console[B]
def transformOutput[B](transform: A => B): Console[B]
```

recap console
transformOutput vs chain
laws?

one example for laws:

readLine.transform(a => s"$a").flatMap(Console.println)

IO.readLine.map(a => s"")

Console.readLine
  .transformOutput { username => s"Hello, $username!" }
  .chain { greeting => Console.print(greeting) }
  

Console.readLine
  .chain { username => Console.emitOutput(s"Hello, $username!" )}
  .chain { greeting => Console.print(greeting) }

Console.readLine
  .chain { username => 
     Console.emitOutput(s"Hello, $username!" )
        .chain { greeting => Console.print(greeting) }
  }

Console.readLine
  .chain { username => Console.print(s"Hello, $username!") }



names

<!-- ## Chaining -->

<!-- then with transformOutput, and expand a bit on why nothing happens (annoying detour to show the printing) -->

<!-- I might split this article in 2: -->
<!-- part I - andThen, chain (and definition) & value/pure, laws?, appendix? -->
<!-- can structure the whole article around one program:  -->
<!-- part II - recap, transformOutput and chain, real names. Possibly move laws here. -->

<!-- ### andThen or transformOutput? -->

<!-- `uppercaseNamePrompt` uses both `andThen` and `transformOutput`, let's -->
<!-- go through a further example to drive home the difference between the -->
<!-- two. -->

<!-- We will write a program that waits for user input, prints a message, then terminates. -->
<!-- The correct way to write this program is via `andThen`: -->

<!-- ```scala -->
<!-- val waitInput: Console[Unit] = -->
<!--   Console -->
<!--     .readLine -->
<!--     .andThen(Console.print("\n Exiting!")) -->
<!-- ``` -->

<!-- but it's interesting to look at this broken version instead: -->

<!-- ```scala -->
<!-- val wrongWaitInput: Console[Console[Unit]] = -->
<!--   Console -->
<!--     .readLine -->
<!--     .transformOutput(_ => Console.print("\n Exiting!")) -->
<!-- ``` -->

<!-- To begin with, it might not be obvious why it compiles, let's look at -->
<!-- it again with explicit type annotations everywhere: -->
<!-- ```scala -->
<!-- sealed trait Console[A] { -->
<!--   def transformOutput[B](transform: A => B): Console[B] -->
<!--   ... -->


<!-- val wrongWaitInput: Console[Console[Unit]] = -->
<!--   (Console.readLine: Console[String]) -->
<!--     .transformOutput[Console[Unit]] { (_: String) => -->
<!--        Console.print("\n Exiting!"): Console[Unit] -->
<!--     } -->
<!-- ``` -->

<!-- Just like in `uppercaseNamePrompt` the `[B]` type parameter of -->
<!-- `transformOutput` assumes the concrete type `String`, in -->
<!-- `wrongWaitInput` `[B]` assumes the type `Console[Unit]`. The type of -->
<!-- the result is therefore `Console[Console[Unit]]`, as you can see by -->
<!-- writing `Console[Unit]` wherever you see `B`in the signature of -->
<!-- `transformOutput`. -->

<!-- TODO the more I think about this, the more I think it should come after explaining chaining -->
<!-- call out `console ; console`, and `console.map(a => console)` as errors. I don't want to introduce `chainNested/flatten`, I'll do it in the combinators instead. I'll chain it explicitly to execute it, to highlight the comparison with `console ; console`. you might have notice similarities between chain and transformoutput (show sigs), let's look at an example to drive home the difference, do example, show why it compiles, how do we understand it? It's a program that _outputs_ another program, what's the behaviour: doesn't print, this is surprising if thinking of side effects, but programs are values, for example if I have `print; print` it wont' execute, I have to chain explicitly. Similarly when I have a program that returns another program, I am free to discard it `nested.transformOutput(_ => ())`, and if I want to execute it I have to chain it explicitly `nested.chain(next => next)`. Therefore, `chain` is fundamentally more powerful than `transformOutput`. -->

<!-- On the other hand, we can look at `transformOutput` as a special case of chaining two programs, where the second one does nothing but a simple transformation of the input, all we need to do is transform `A => B` into `A => Console[B]` to make fit blah blah -->

<!-- and then find a sentence for the transformOutput digression, worth analysing another example to fully understand the difference between andThen and trasformOutput, we will use the following program: waits for the user to insert any input, print a message, and terminate, the correct way to write it is with andThen, what happens if I use tranformOutput instead? (example, show why it compiles, explain what it does, it's just an output). Move "we will explore..." at the end of the first section -->

<!-- introduce chainNested (possibly after pure) -\-> nope -->

<!-- a rose by any other name -->
<!-- real names, laws -->
<!-- rewrite examples with flatMap -->



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


