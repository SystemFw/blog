# Programs as Values, Part 0: Intro & Compositionality

This is the first post in a series about a programming paradigm I like
to call __programs as values__.

In Haskell and part of the Scala community, it is also known as purely
functional programming (or pure FP), but I will not use that term
here, to avoid any ambiguity with the discussion about _imperative_ vs
_declarative_ code, which are both possible within programs as values.

I also wish to distinguish programs as values from other approaches to
functional programming such as __effects and handlers__ as seen in
Unison, Eff or Koka, which I might talk about in the future.

I will make the following assumptions about my readers, based on my
typical audience:

- You know Scala syntax, as well as FP basics like ADTs, recursion,
  higher order functions and pattern matching.
- You're are vaguely aware of the existence of libraries like
  [cats-effect](https://github.com/typelevel/cats-effect) and
  [fs2](https://github.com/functional-streams-for-scala/fs2), for
  example through one of my [talks](https://systemfw.org/talks).
- You're looking for a deeper, lasting understanding of the core ideas
  behind programs as values, beyond the specifics of a single library.


## Compositionality

Let's start from the concept of compositionality, which is the idea
that we can understand the __whole__ by understanding the __parts__
and the __rules of composition__.

The key characteristic of compositional systems is that they can be
decomposed into parts that still make sense on their own and,
specularly, they can be built by assembling smaller parts together.  
This fits well with the limited capacity of our brains: if I asked you
to build a model of the Colosseum, would you rather assemble it with
Lego, or sculpt it with marble?

To see how the concept applies to software, we will consider this
simple example program:

> repeatedly print "hello", stopping after 10 iterations, once per
> second


We can implement the above program with:

```scala mdoc:compile-only
var i = 0
while(i < 10) {
 Thread.sleep(1000)
 println("hello")
 i = i + 1
}
```

but note how that's not very compositional. First of all, two logical
subprograms cannot be combined without changes:

```scala mdoc:compile-only
def p1 = {
 var i = 0
 while(i < 10) {
  println("hello")
  i = i + 1
 }
}

def p2 = Thread.sleep(1000)

def p3 = ??? //combine p1 and p2?
```
And second, not all the constituent parts make sense on their own, for example `i = i + 1` only makes sense when considered together with `var i = 0` and `while(i < 10)`.

Low compositionality is fine in small doses, but it scales poorly with
complexity: even a slightly more complex version of the program (e.g
stop after 10 seconds or 10 iterations, waiting a random interval
between iterations) leads to more entangled code.  
In the extreme, it will result in spaghetti code, code which has such
a low level of compositionality that it can't be separated into its
constituent parts at all.

Compare instead with this alternative solution, which uses
`fs2.Stream`:

```scala
val p = IO(println("hello"))

Stream.repeatEval(p)
      .take(10)
      .metered(1.second)
```

This code is highly compositional, it is made of smaller parts, which
all make sense as individual programs:

- `IO(println("hello"))` is the program that prints "hello".
- `repeatEval` is the program that executes another program
  indefinitely.
- `take(n)` is the program that evaluates the first `n` iterations of
  another program.
- `metered` is the program that executes another program at the given
  rate.

Even without understanding all the details, it should be clear that it
maintains this compositional quality in the more complex example too:

```scala
def randomWait =
  Stream
    .random[IO]
    .evalMap(n => IO.sleep(n.nanos))

def hello = IO(println("hello"))

Stream
  .repeatEval(hello)
  .zipLeft(randomWait)
  .take(10)
  .interruptAfter(10.seconds)
```

## Conclusion

The two examples above are rather extreme, and may give the impression
that compositionality is a binary attribute. In reality, it's on a
spectrum: the more compositional our software is, the better we can
cope with its complexity.   
And that leads me to the main point of this post: what is programs as
values about?

__Programs as values is about removing barriers to compositionality.__




## Programs as Values, Part 1: Doing & Being





definitely non composable, need to find out compositionality example
basically the parts do not make sense on their own, but only with the
context of other parts

then fs2


analyis, build small programs, then assemble them also show how to
decompose, each program makes sense on its own

The two examples are rather extreme, gives the impression of a binary
property, but it's really nuanced, programs are compositional


this is series is primarily about effects 2 fundamental questions
doing vs being recontextualising effects

briefly mention the first, lot of talk about it, and we might indeed
talk about it as well but let's focus on doing vs being "hello" +
readLine repeat 5 readLine

def vs val

doing: sequencing being a value: referential transparency

core idea: being only, unlink evaluation, data, explicit combination


algebraic structure: intro form, combinators, elimination forms

progression: monoid, why F[A], functor, (split here?) monad,
monaderror
