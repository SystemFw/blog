---
title: "Programs as Values, Part IV: Algebras"
date: 2022-01-04
---

In the previous instalments of this series, we've introduced two core
components of the programs as values paradigm: *datatypes* that
represent effects, and *functions* that combine instances of those
datatypes in order to construct programs by describing control flow
explicitly.

Today we will formalise and clarify this structure, by introducing the
concept of an **algebra**.

> Note: In an unfortunate abuse of notation, the word "algebra" is
> used with a few different meanings in the FP world. See the Appendix
> for more details on the terminology.

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

```scala
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

```scala
val output: Doc = 
  Doc
    .text("Directions:")
    .line(directions.indent(2))
    .bracketBy(Doc.space, Doc.space)
```

and once we're done, we can "run" our `Doc` program by translating it
to `String` via the `render` elimination form:

```scala
output.render(width = 30)
// res0: String = """ 
//   Directions:
//     [ NORTH EAST SOUTH WEST ]
//  """
output.render(width = 15)
// res1: String = """ 
//   Directions:
//     [
//       NORTH
//       EAST
//       SOUTH
//       WEST
//     ]
//  """
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

It's significant that we can write `Out` programs without any
knowledge of its internal structure: we didn't do any pattern matching
on the concrete case classes that form `Out`, but only used the
operations defined on it, or, in other words, its algebra. This style of
_algebraic thinking_ is very valuable, because it scales from
datatypes with very simple internal structure, such as `Option`, all
the way to highly sophisticated datatypes such as `IO` or
`fs2.Stream`.

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
ability to handle _outputs_. See you then!

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

Universal Algebra also introduces the concept of a _signature_, which
defines the set of typed operation symbols without specifying the
functions that would be the actual operations. Signatures correspond
roughly to the notion of "interface" in programming, and are often
encoded in Scala via typeclasses.

The issue is that FP terminology uses the word "algebra" to also mean
"signature", for example in the phrase "algebras and
interpreters" which roughly corresponds to "abstractions and
implementations" (the usage of the word "interpreter" comes from the
theory of embedded domain specific languages, or eDSLs).
