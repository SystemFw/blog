---
title: "Programs as Values, Part VI: Chaining"
date: 2022-01-10
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

val inputLength: Console[Int] =
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
    .print("What's your user name? ")
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
    .print("What's your name? ")
    .andThen(Console.readLine)
```

Next step is to create the greeting message, which sounds like a job for
`transformOutput`:

```scala
val promptWithGreeting: Console[String] =
  Console
    .print("What's your name? ")
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
val input: String = readLine()
println(input)
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
  Console.readLine.chain { input =>
    Console.print(input)
  }

// Same, but with explicit annotations for every type:
val echo: Console[Unit] = 
  // chain: (Console[String], String => Console[Unit]) => Console[Unit] 
  (Console.readLine: Console[String]).chain { 
     (
       (input: String) => 
          Console.print(input): Console[Unit]
     ): String => Console[Unit]
  }: Console[Unit]

```

and indeed express our target program:

```scala
val promptAndGreet: Console[Unit] =
  Console
    .print("What's your name? ")
    .chain { _ => Console.readLine }
    .transformOutput { username => s"Hello, $username!" }
    .chain { greeting => Console.print(greeting) }
```

Note that in `promptAndGreet` we've replaced `print.andThen(readLine)`
with `print.chain { _ => readLine}`, i.e. `andThen` is a special case
of `chain` where the shape of the next program doesn't depend on the
output of the previous one, and can ignore it.

So, we've clearly gained some power with `chain`, but how much exactly?
After all, `chain` and `andThen` are not that different:

```scala
chain[A, B]  : (Console[A], A => Console[B]) => Console[B]
andThen[A, B]: (Console[A],      Console[B]) => Console[B]
```

As it turns out though, we've gained a huge amount: `next: A =>
Console[B]` can use the output of a previous computation in
_arbitrary ways_ to decide what the next computation should be. In
`nameAndGreet` we simply passed that output through, but `next` could
include `if/else` expressions, recursion, pattern matching... or in other
words, _general control flow_.

## pure/value

<!-- ---- -->

<!-- I'm now going to introduce the algebra that will accompany us for the -->
<!-- next few instalments of this series, the `Console` algebra. We will -->
<!-- start with an imperfect version, and iterate: -->

<!-- ```scala -->
<!-- /* -->
<!--  * carrier: -->
<!--  *   Console[A] -->
<!--  *     where A represents the output of a Console program -->
<!--  * introduction forms: -->
<!--  *   readLine: Console[String] -->
<!--  *   print: String => Console[Unit] -->
<!--  * combinators: -->
<!--  *   andThen[A]: (Console[A], Console[A]) => Console[A] -->
<!--  *   transformOutput[A, B]: (Console[A], A => B) => Console[B] -->
<!--  * elimination forms: -->
<!--  *   run[A]: Console[A] => IO[A] -->
<!--  */ -->
<!--  sealed trait Console[A] { -->
<!--    def andThen(next: Console[A]): Console[A] -->
<!--    def transformOutput[B](transform: A => B): Console[B] -->

<!--    def run: IO[A] -->
<!--    ... -->
<!--  object Console { -->
<!--    val readLine: Console[String] -->
<!--    def print(s: String): Console[Unit] -->
<!--    ... -->
<!-- ``` -->
<!-- although we will ignore the `run` elimination form for the remainder -->
<!-- of the article, and focus on writing programs with `Console`. -->

<!-- `readLine` and `transformOutput` have a familiar shape, but `print` -->
<!-- and `andThen` need to fit the `Console[A]` shape, so we use the `Unit` -->
<!-- type to express that printing has no meaningful output: -->

<!-- ```scala -->
<!-- def print(s: String): Console[Unit] -->
<!-- ``` -->

<!-- and parameterise `andThen` with `A` everywhere: -->

<!-- ```scala -->
<!-- // andThen[A]: (Console[A], Console[A]) => Console[A] -->
<!-- sealed trait Console[A] { -->
<!--    def andThen(next: Console[A]): Console[A] -->
<!--    ... -->
<!-- ``` -->

<!-- and we can write `Console` programs! -->

<!-- ```scala -->
<!-- val helloWorld: Console[Unit] = -->
<!--   Console.print("Hello ").andThen(Console.print("World!")) -->

<!-- val upperCaseInput: Console[String] = -->
<!--   Console.readLine.transformOutput(line => line.toUpperCase) -->
<!-- ``` -->

<!-- > Note that techniques to compose existing effects are out of scope -->
<!-- for now, so we've defined `Console` from scratch even though its -->
<!-- functionality is a combination of -->
<!-- [Out](https://systemfw.org/posts/programs-as-values-IV.html) and -->
<!-- [In](https://systemfw.org/posts/programs-as-values-V.html). -->

<!-- We will now explore and evolve our `Console` algebra by writing some -->
<!-- programs with it. -->


<!-- ## Generalising andThen -->

<!-- Our first program will ask the user for their name, read it from -->
<!-- stdin, and convert it to upper case. Let's start with the prompt: -->

<!-- ```scala -->
<!-- val namePrompt: Console[String] = -->
<!--   Console -->
<!--     .print("What's your name? ") -->
<!--     .andThen(Console.readLine) -->

<!-- type mismatch; -->
<!-- [error]  found   : Console[String] -->
<!-- [error]  required: Console[Unit] -->
<!-- [error]       .andThen(Console.readLine) -->
<!-- [error]                        ^ -->
<!-- ``` -->

<!-- Uh-oh, it doesn't compile: `andThen` wants both arguments to be -->
<!-- `Console` programs with the same type of output, but `print(s)` and -->
<!-- `readLine` have different output types, respectively `Console[Unit]` -->
<!-- and `Console[String]`. -->

<!-- This limitation doesn't seem reasonable, so let's relax the type of -->
<!-- `andThen` to allow the second program to have a different output type, -->
<!-- which will also be the output type of the overall expression: -->

<!-- ```scala -->
<!-- // andThen[A, B]: (Console[A], Console[B]) => Console[B] -->
<!--  sealed trait Console[A] { -->
<!--    def andThen[B](next: Console[B]): Console[B] -->
<!--    ... -->
<!-- ``` -->
<!-- and we can write `namePrompt` unchanged: -->

<!-- ```scala -->
<!-- val namePrompt: Console[String] = -->
<!--   Console -->
<!--     .print("What's your name? ") -->
<!--     .andThen(Console.readLine) -->
<!-- ``` -->
<!-- and complete our program: -->

<!-- ```scala -->
<!-- val uppercaseNamePrompt: Console[String] = -->
<!--   Console -->
<!--     .print("What's your name? ") -->
<!--     .andThen(Console.readLine) -->
<!--     .transformOutput(name => name.toUppercase) -->
<!-- ``` -->

<!-- ## Chaining -->

<!-- program 2: write prompt, then read, convert to upper case, print upper case -->
<!-- try with andThen, and show compile error -->
<!-- then with transformOutput, and expand a bit on why nothing happens (annoying detour to show the printing) -->
<!-- andThen gives a starting point, recall from part III -->
<!-- introduce chain -->


<!-- I should make the point that once you have `A =>` you have a lot more power than just passing the argument, -->
<!-- the entirety of sequential control flow is available to you -->
<!-- need an example for pure, example: retry, read a line, only return it if less than 10 characters -->


<!-- I might split this article in 2: -->
<!-- part I - andThen, chain (and definition) & value/pure, laws?, appendix? -->
<!-- can structure the whole article around one program:  -->
<!-- part II - recap, transformOutput and chain, real names. Possibly move laws here. -->

<!-- ask for name, read it, convert to uppercase, and output. If name is empty, should just retry. -->
<!-- If I want to use pure, I need to make the point about retryOnEmpty combinator or it won't be needed, just -->
<!-- have both programs in an if. Also I won't bother with prompts like "empty strings not allowed", just keep it simple -->


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



<!-- ^^ should I write this as the first program, in english, but say "let's start" with the prompt, -->
<!-- so then after changing `andThen` I can say: and we can now write our full program, and insert the above, -->
<!-- and then find a sentence for the transformOutput digression, worth analysing another example to fully understand the difference between andThen and trasformOutput, we will use the following program: waits for the user to insert any input, print a message, and terminate, the correct way to write it is with andThen, what happens if I use tranformOutput instead? (example, show why it compiles, explain what it does, it's just an output). Move "we will explore..." at the end of the first section -->

<!-- skip this -->
<!-- Console algebra, program 0.5 -->
<!-- write prompt, then read, convert to upper case -->
<!-- important aside here to show how andthen and map are different with print. Make point about laws as _understanding aid_ . Example I can use: two prints or read >> "you started!" -->


<!-- program 2: write prompt, then read, convert to upper case, print upper case -->
<!-- try with andThen, and show compile error -->
<!-- then with transformOutput, and expand a bit on why nothing happens (annoying detour to show the printing) -->
<!-- andThen gives a starting point, recall from part III -->
<!-- introduce chain -->
<!-- introduce chainNested (possibly after pure) -\-> nope -->

<!-- I should make the point that once you have `A =>` you have a lot more power than just passing the argument, -->
<!-- the entirety of sequential control flow is available to you -->
<!-- need an example for pure, example: retry, read a line, only return it if less than 10 characters -->


<!-- a rose by any other name -->
<!-- real names, laws -->
<!-- rewrite examples with flatMap -->

<!-- appendix: -->
<!-- show the console ADT, but do remark we will mostly ignore the -->
<!-- structure. Maaaybe link the fiber talk. I'm actually not sure I want -->
<!-- to do this. I could do it in the IO articles instead, as an -->
<!-- introduction to IO. I could do a small version: translate to IO, and -->
<!-- then say "how does IO do it: we will talk about inthe appropriate -->
<!-- article" -->


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
