---
title: "Programs as Values, Part IV: Algebras"
date: 2021-12-29
---

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
  construct bigger programs, often by explicit describing some form of
  control flow. Most of the logic in your code will be expressed
  through combinators.
  
4) A set of functions called **elimination forms**. They can all take
different inputs, but one of those inputs needs to be of the carrier
type `T`. Furthermore, their output needs to be a *different type*
from `T`, i.e. they *eliminate* `T`.  
Informally, elimination forms encode the concept of "running" a
program written in a specific algebra after we've done constructing it.
The concept of running is however expressed as a *translation* from
the carrier type `T` to another type.
  
  
In some cases, algebras also have a fifth component: **laws**, i.e.
equalities between programs written in the algebra. We won't be
talking much about laws in this post, but they will be more prominent
later.
  
So to recap, to build a program in a given algebra, we start from the
introduction forms, and use combinators to express all the logic we
need. Once we're done, we run it by translating it to another type via
one of the elimination forms. Another way to look at it is that each
algebra describes a *mini language*: we write programs in that
language, and then translate them to another language, all the way to
the most general language of all, which is, as we will see, `IO`.

We're now ready to look at some concrete examples.

## Pretty printing




  
  
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
