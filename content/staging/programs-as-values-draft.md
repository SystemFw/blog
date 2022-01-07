---
title: "Programs as Values, Part V: Algebras with results"
date: 2022-01-04
---


at some point I should say: no elimination forms in this article
`In` algebra: read a String, count it's length, return false/true whether it's greater than 10
introduce the necessity for type params

Console algebra, program 1
write prompt, then read, convert to upper case

program 2: monads

hello
In the previous instalments of this series, we've introduced two core components of the programs as values paradigm: *datatypes* that represent effects, and *functions*  that combine instances of those datatypes in order to construct programs by describing control flow explicitly.

Today we will formalise and clarify this structure, by introducing the concept of an **algebra**.

> Note: In an unfortunate abuse of notation, the word "algebra" is used with a few different meanings in the FP world. See the Appendix for more details on the terminology.

An algebra is a structure consisting of four components:

 1) A single, concrete type `T`, which is sometimes called the **carrier** of the
  algebra. Depending on the algebra, it can have different shapes: `V`,
  `F[A]`, `G[E, R]`, etc. The carrier type is the datatype that
  represents the particular effect its algebra encodes.

2) A set of functions which return values of the carrier type `T`, called
  **introduction forms**. These functions can all take different
  inputs, but what characterises them as introduction forms is that
  none of their inputs is of the carrier type `T`, i.e. they
  *construct* or *introduce* `T`. Introduction forms are the *primitive programs* of
  their algebra, the starting points from which we can build more
  complex logic.

 3) A set of functions which return values of the carrier type `T`, called
  **combinators**. These functions can all take different inputs, but
  at least one of those inputs needs to be of the carrier type `T`. In
  other words, a combinator takes one or more programs in `T`, and
  returns another program in `T`.  
  Combinators are the essential building blocks of programs as values
  we've been talking about so far, they take smaller programs and
  construct bigger programs, often by explicitly describing some form of
  control flow. Most of the logic in your code will be expressed
  through combinators.
  
4) A set of functions called **elimination forms**. They can all take
different inputs, but one of those inputs needs to be of the carrier
type `T`. Furthermore, their output needs to be a *different type*
from `T`, i.e. they *eliminate* `T`.  
Informally, elimination forms encode the concept of "running" a
program written in a specific algebra after we're done constructing it.
The concept of running is however expressed as a *translation* from
the carrier type `T` to another type.
  
  
In some cases, algebras also have a fifth component: **laws**, i.e.
equalities between programs written in the algebra. We won't be
talking much about laws in this post, but they will be more prominent
later.
  
So to recap, to build a program in a given algebra we start from the
introduction forms, and use combinators to express all the logic we
need. Once we're done, we run it by translating it to another type via
one of the elimination forms. Another way to look at it is that each
algebra describes a *mini language*: we write programs in that
language, and then translate them to another language, all the way to
the most general language of all, which is, as we will see, `IO`.

We're now ready to look at some concrete examples.

## The Doc algebra

For our first example, we're going to look at
[paiges](https://github.com/typelevel/paiges), a pretty printing
library that lets you generate text that needs to be well formatted,
with paragraph wrapping at different line lengths. The entire library
is centred around a document algebra, so let's look at its components
in more detail.

The carrier type of the algebra is called `Doc`, which means we
represent documents as programs of type `Doc`, like `val myDoc: Doc`.
It might sound strange that we're referring to a static artifact such
as a document as a "program", but you will see that the mindset we're
working in doesn't change that much once we move to dynamic behavior
with types such as `cats.effect.IO` or `cats.parse.Parser`.


The introduction forms are the initial building blocks, as per their
definition they create values of type `Doc` without requiring any
value of type `Doc` as input.
Here are a few:

```scala
Doc.text: String => Doc
Doc.char: Char => Doc
Doc.spaces: Int => Doc
Doc.comma: Doc
Doc.line: Doc
... // and more
```


Note that I'm showing them as functions to make the types clearer,
but they are generally methods in Scala, so:

```scala
val char: Char => Doc
// is more typically written as
def char(c: Char): Doc
```

also, they tend to appear in the companion object of the carrier type,
but don't use that as a strict rule, sometimes combinators are there
too.

Introduction forms have shape `(A, B, ..) => Doc`, so you might be
wondering where do primitive values such as `comma` or `line` fit. You
can think of them as introduction forms from `Unit` to the carrier:

```scala
val comma: Doc
// can be thought as
val comma: () => Doc
```

Let's now look at combinators, functions that take one or more values
of type `Doc` and return a value of type `Doc`. Combinators will
express the lion's share of our logic, so much so that libraries based
on algebras are often called _combinator libraries_. Here are a few:

```scala
indent: (Doc, Int) => Doc
line: (Doc, Doc) => Doc
bracketBy: (Doc, Doc, Doc, Int) => Doc
aligned: Doc => Doc
stack: Iterable[Doc] => Doc
... // and more
```
Again, I wrote them as standalone functions to make the types clearer,
in reality they are written as instance methods, which obscures the
fact that at least one of their inputs (`this`) is of type `Doc`:

```scala
sealed trait Doc {
  def indent(i: Int): Doc
  def line(that: Doc): Doc
  def bracketBy(left: Doc, right: Doc, indent: Int = 2): Doc
  def aligned: Doc
  ... 
  
object Doc {
  def stack(ds: Iterable[Doc]): Doc
  ... 
```

`Doc.stack` is a notable exception, it cannot be an instance method on
`Doc` because it operates on a _collection_ of docs. The same
intuition about combinators applies though: `stack` combines programs
in the `Doc` algebra into a bigger program in the `Doc` algebra.

Finally, eliminations forms translate `Doc` programs to another type:

```scala
isEmpty: Doc => Boolean
maxWidth: Doc => Int
render: (Doc, Int) => String
```
with the usual caveat that they are defined as instance methods:

```scala
sealed trait Doc {
  def isEmpty: Boolean
  def maxWidth: Int
  def render(width: Int): String
```


Equipped with this knowledge, we can now write `Doc` programs:

```scala mdoc:silent
import org.typelevel.paiges.Doc

val openBracket: Doc = Doc.char('[')
val closeBracket: Doc = Doc.char(']')

val north: Doc = Doc.text("NORTH")
val east: Doc = Doc.text("EAST")
val south: Doc = Doc.text("SOUTH")
val west: Doc = Doc.text("WEST")

val directions: Doc = 
  Doc
    .stack(List(north, east, south, west))
    .bracketBy(openBracket, closeBracket)
```

In the above, I've separated usages of introductions forms from
combinators for extra clarity, but remember that referential
transparency holds, so you can inline definitions or abstract them out
freely. We can keep composing to build more complex programs:

```scala mdoc:silent
val output: Doc = 
  Doc
    .text("Directions:")
    .line(directions.indent(2))
    .bracketBy(Doc.space, Doc.space)
```

and once we're done, we can "run" our `Doc` program by translating it
to `String` via the `render` elimination form:

```scala mdoc
output.render(width = 30)
output.render(width = 15)
```

## The Out algebra

Let's now look at a made up algebra we're going to call `Out`, whose
_only_ capability is to print Strings to stdout. It should provide a
first, simple example of encoding behaviour, which is more in line
with how we think about the word "program".

Here's how it looks like:

```scala
// Carrier type
Out
// Introduction forms
print: String => Out
// Combinators
andThen: (Out, Out) => Out
// Elimination forms
run: Out => IO[Unit]
```

except we're going to assume it's written idiomatically, with
`andThen` as an instance method and `print` as a method in the
companion object of `Out`.

Let's write a program with it:

```scala
object Logic {
  val printNewline: Out = Out.print("\n")
  
  val helloWorld: Out = 
    Out
      .print("Hello, world!")
      .andThen(printNewLine)
      .andThen(Out.print("I'm a program!"))
}

object Main extends IOApp.Simple {
  def run: IO[Unit] = Logic.helloWorld.run
}
```
```scala
Main.main(Array())

// Hello, world!
// I'm a program!
```


So the `Out` algebra lets us create simple imperative programs that
are akin to:

```scala
print("tea")
print("coffee")
```

except we're in programs as values, so we can easily write
compositional code as programs that manipulate other programs.
Here's a basic example, we will see much more interesting ones once
we work with algebras that encode complex control flow:

```scala
def repeat(p: Out, n: Int): Out =
  if n <= 0 Out.print("")
  else p.andThen(repeat(p, n - 1))
```

It's significant that we can use `Out` without any knowledge of its
internal structure: we only need to know the operations defined on it,
or, in other words, its algebra. This style of _algebraic thinking_ is
very valuable, because it scales from datatypes with very simple
internal structure, such as `Option`, all the way to highly
sophisticated datatypes such as `IO` or `fs2.Stream`.

Finally, note that there really is no difference in mechanics nor
mindset between imperative-looking algebras such as `Out`, and
algebras that encode static data such as `Doc`. In fact, we can
literally implement `Out` with `Doc`:

```scala
type Out = Doc
def print(s: String): Out = Doc.text(s)
def andThen(this: Out, that: Out): Out = this + that
def run(p: Out): IO[Unit] = IO.println(p.render(Int.MaxValue))
```


## Conclusion

In this article, we've talked about algebras, the fundamental
structure at the heart of programs as values. Next time is when things
really get interesting, as we're going to enrich our algebras with the
ability to handle _results_. See you then!

---

## Appendix on terminology

Terminology can be a source of confusion, since it's used loosely and
inconsistently and mostly reflects historical connections between
fields and communities.

"Algebra" is a particularly overloaded term: the meaning we used in
the article, which also appears in "Boolean algebra" and "Algebraic
Data Types", originates from a branch of Maths called Universal
Algebra.
It's defined as:

- A set `A`, called the carrier.
- Some _finitary operations_ on `A`: functions that take tuples of
  elements of `A` and return an element of `A`.

which we called "carrier" and "combinators" respectively. The terms
"introduction forms" and "elimination forms" come from logic via type
theory instead.

Universal Algebra also introduces the concept of a signature algebra,
or _sigma algebra_, which defines the set of typed operation symbols
without specifying the functions that would be the actual operations.
Sigma algebras correspond roughly to the notion of "interface" in
programming, and are often encoded in Scala via typeclasses.

The issue is that FP terminology uses the word "algebra" to also mean
"sigma algebra", for example in the phrase "algebras and
interpreters" which roughly corresponds to "abstractions and
implementations" (the usage of the word "interpreter" comes from the
theory of embedded domain specific languages, or eDSLs).


<!-- The point about derived and primitive things is not important for this article, it's really more about how algebras are implemented. The point of this article is recognising the algebraic structure, so I'm not going to include it

<!-- Finally, note that some introduction forms might be derived from others: -->
<!-- ```scala -->
<!-- val comma: Doc = Doc.char(',') -->
<!-- ``` -->

<!-- but don't just assume that's the case. For example in `paiges` -->
<!-- `Doc.line` is _not_ defined as `Doc.text("\n")`, because the -->
<!-- implementation of the library needs extra structure to deal with line -->
<!-- breaks. (insert something about high perf?) -->

<!-- The point is, however, that we don't need to know the internal -->
<!-- structure of an algebra to work with it, and as a matter of fact the -->
<!-- entire concept of thinking _algebraically_ can be thought as thinking -->
<!-- in terms of _operations_ instead. In this light, we can also show the -->
<!-- usefulness of laws as an aid in reasoning, for example the library -->
<!-- implementor might state, and ensure, that: -->

<!-- ```scala -->
<!-- Doc.line <-> Doc.text("\n") -->
<!-- ``` -->

<!-- libraries authors can define laws, which are equivalences between programs, auch -->
<!-- laws hold, e..g Doc.line <-> Doc.text("\n") (add examples). Sometimes something like line is literally defined as Doc.text, sometimes not -->

<!-- Questions: -->
<!-- - where to put the idea that some ops are defined in terms of others? -->
<!-- - talk about reasons to do this like performance? -->
<!-- - talk about laws? -->
<!-- - The point about algebraic thinking is important and not really that related -->
<!--   to the discussion about derived operations, probably put it at the end of the whole article -->
<!-- - derived ops and laws maybe need to be discussed on their own, not as a throway remark here. -->





  
  
<!-- We've spent the first few instalments of this series discussing the core idea behind the programs as values paradigm: representing effectful programs as datatypes, and assembling big programs out of smaller one via functions that explicitly describe control flow -->

<!-- In programs as values, effects are represented as datatypes, and today we will be looking at the structure formed by these datatypes, by introducing the concept of an **algebra**. -->


<!-- use Doc as first example, then Log, including repeat -->

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

<!-- after results: combinator overview up to monad? -->
<!-- errors -->
<!-- when do I do traverse? its own article: effectful iteration -->
<!-- IO & FFI -->
<!-- Resource? (when? how? do I talk about concurrency/interruption first?) -->
<!-- combinators overview? (up to Traverse), but then what about parallel? -->

<!-- ---------- -->
<!-- part ?? : effectful constructors -->
<!-- trait A { -->
<!--  def foo: B -->
<!-- } -->
<!-- point about how to name these abstractions? remark the point about algebras being overloaded. Probably going to call the interfaces -->
<!-- class MyA(...) extends A -->
<!-- def myA(...): A = new A { -->
<!-- } -->
<!-- version with IO: -->
<!-- then show constructors with effects, and resource -->
<!-- stateful constructors (explain allocation is mutable? or just that you will receive it) -->
<!-- constructors that initialise a resource -->
<!-- resource that spawn concurrent processes -->
<!-- constructors that reuse other constructors that are effectful -->
<!-- regions of sharing -->
<!-- --------------- -->

<!-- -------- -->
<!-- part ??: iteration -->
<!-- introduce List as abstract, intro form, some combinators (map, -->
<!-- flatMap, zip, takeWhile), foldRight. Try doing `List(Questions) => -->
<!-- Console[List[Outputs]]`, show the shape of `traverse` on `List`, -->
<!-- possibly `sequence` as well, then say we generally don't look at -->
<!-- implementations, but look no magic: foldRight + `pure/mapN`. Then show -->
<!-- the same for Option. Make a small point about "You might be wondering -->
<!-- if these can be abstracted, much the same as with the combinators on -->
<!-- F-A-M, yes but out of scope for now" > -->
<!-- -------- -->

<!-- ------- -->
<!-- possible plan:  -->
<!-- results, V  -->
<!-- errors, VI -->
<!-- iteration, VII -->
<!-- combinator deluge, VIII -->
<!-- IO & FFI IX -->
<!-- basic concurrency? parMapN, race, sleep, timeout (possibly parTraverse)? -->
<!-- resource (add an example restating referential transparency, possibly right before part XII) XI -->
<!-- effectful constructors XII -->
<!-- advanced concurrency/state -\-> should this be a separate series? XIII -->
<!-- abstraction? should this be a separate series? -->
<!-- ------- -->

<!-- ------- -->
<!-- sources for algebras -->
<!-- https://okmij.org/ftp/tagless-final/Algebra.html -->
<!-- https://books.google.it/books?id=MS2f1AATHIoC&pg=PA267&lpg=PA267&dq=with+a+finite+set+of+total+functions+that+have+the+carrier+set+as+their+common+codomain.&source=bl&ots=rRTtRtO-hY&sig=ACfU3U1b8lOc189R8gaOSEzlFjjmXYHBKA&hl=en&sa=X&ved=2ahUKEwiOpcuh75b1AhUJM-wKHdxBBpoQ6AF6BAgREAM#v=onepage&q=with%20a%20finite%20set%20of%20total%20functions%20that%20have%20the%20carrier%20set%20as%20their%20common%20codomain.&f=false -->
<!-- https://en.wikibooks.org/wiki/Universal_Algebra/Definitions,_examples -->
<!-- ------- -->
