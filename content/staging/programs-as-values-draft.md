# Programs as Values, Part I: Intro & Compositionality

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



definitely non composable, need to find out compositionality example
basically the parts do not make sense on their own, but only with the
context of other parts

then fs2


analyis, build small programs, then assemble them also show how to
decompose, each program makes sense on its own

The two examples are rather extreme, gives the impression of a binary
property, but it's really nuanced, programs are compositional


## Programs as Values, Part II: Doing & Being

A large part of programming in many domains is dominated by
_computational effects_ such as concurrent state or I/O, yet one
often hears that functional programming is about limiting or
eliminating them. 

As it turns out though, programs as values has a lot to say about
effects, and the common refrain of "having a non-effectful core with
effects at the edges" is not a constraint of the paradigm, but merely
architectural advice, and like all advice it is sometimes useful, and
sometimes harmful.

In fact, I like programs as values _because_ I have to deal with
effects, not in spite of it, and they will be the focus of this
series.

Effectful code introduces a few fundamental problems:
- Controlling when an effect happens.
- Changing the meaning of an effect based on contest (e.g. in a test)
- Mixing different effects.

A lot has been written about the last two points, but here I want to
focus on the first, and introduce the notion of __doing__ vs
__being__.

## Doing & Being

Let's look at these two snippets.

```scala
"hello".append(readLine)
```
```scala
repeatAction(times = 3, readLine)
```


The exact definition of `append` and `repeatAction` does not matter
too much, pay attention to the use of `readLine` instead, which is
very different:
- In the first snippet, we want to _execute_ `readLine` and pass its
  _result_ to `append`.
- In the second snippet, we don't want to pass `readLine`'s result to
  `repeatAction`, but `readLine` _itself_, without executing it.
  `repeatAction` can then use it internally as it pleases (in this
  case by executing it 3 times).

I'll call these two modes of use `doing` and `being` respectively.
`doing` is in a sense the essence of effectful computation, i.e.
executing actions and using their results in subsequent actions.
`being` instead describes passing things to functions, giving them
names, returning them as results, or putting them in data structures.
In other words, it describes _values_.

Generally we tend to associate `being` with non-effectful things like
`5` or `"hello"`, but applying this principle to effectful actions
enables **compositional APIs** : `repeatAction` is only possible when
we use `readLine` in the `being` mode.

We can then compare paradigms for effectful computation in terms of
how they approach `doing` vs `being`, but before doing that, let's
define a bit more precisely what it means for something to be a
_value_.

## Referential transparency

Let's say we want to evaluate this simple mathematical expression

```
(x + 3) + (x * 2)  where x = 2
```

what we do is replace `2` wherever we see `x`, yielding

```
(2 + 3) + (2 * 2)
```

to which we then apply reduction rules to get to `9`.

This process is called the __substitution model of evaluation__, since
it's centred around the concept of _substitution_: replacing names
with the expression they refer to. We typically don't think about
programs this way though, using instead a state machine model where
we go line by line and update the relevant state, as shown in this
snippet:

```scala
val x : String = "Hello".reverse // x is "olleh"
val y : String = x ++ x // y is "olleHolleH"
// final result is "olleHolleH"
```

However,  we can try using the substitution model instead (by replacing
`x` with `"Hello".reverse` everywhere), and nothing changes:

```scala
val y : String = "Hello".reverse ++ "Hello".reverse 
// final result is "olleHolleH"
```

This property is called __referential transparency__: _replacing an
expression with its bound value does not alter the observable
behaviour of the program_. In other words, an expression is
referentially transparent if it respects the subtitution model of
evaluation. We are going to call such expressions __values__.

Referential transparency (RT from now on) captures the essence of
being a value, which is the fact that values require only _local_
reasoning: no matter where a value is used, it is entirely
characterised by its definition, as evidenced by the fact that you can
entirely replace it with such definition without altering the
behaviour of the program.

On a practical level, RT gives us a _systematic_ way to understand
code: just replace names with definitions until you are at a
sufficient level of detail. In the other direction, it also lets us
_forget_ about the details of a given piece of code: just give that
piece a name, and use that name everywhere without fear of breaking
anything. Inlining and name abstraction are always valid with RT
code, and they are useful refactoring tools, in addition to being
highly compositional.

Let's now give a couple of examples where RT does not hold, starting
by evaluating this snippet in the state machine model:

```scala
// it is an iterator over the sequence 1,2,3...
val a = it.next // a is 1
val b = a + 1   // b is 2
a + b // final result is 3
```

if we apply the substitution model, we get this instead

```scala
val a = it.next
val b = a + 1
a + b

// replace the definition of b

val a = it.next
a + a + 1

// so far nothing changes
// let's replace the definition of a

it.next + it.next + 1 // final result is 4
```

The final behaviour is not the same! Another one:

```scala
val b = {
 println("hello") // "hello" gets printed, () is returned
 println("hello") // "hello" gets printed, () is returned
 3 // final result is 3
}

```

let's apply the substitution model in reverse by giving a name to
`println("hello")`

```scala
val b = {
 val p =  println("hello") // "hello" gets printed, () is returned
 p // () is returned
 p // () is returned
 3 // final result is 3
}
```

Behaviour changes again! We say that `it.next` and `println("hello")` are RT breaking


The above might sound obvious, so let's give two examples where RT does not hold

The fact that we can always inline or abstract with a name i not only highly compositional, but also a useful
refactoring tool.




This is highly consistent with the idea of compositional understanding: we can understand individual parts by substitution, and understand the whole without worrying about the details of individual parts.

we can see that th final behaviour of this code hasn't changed when applied substitu the same 


This process is called the __substitution model of evaluation__, since
it's centred around the concept of _substitution_: replacing names
with the expression they refer to.

We typically don't think about code this way, but we can try applying
substitution there as well, for example given this snippet

```scala
val x : String = "Hello".reverse // x is "olleh"
val y : String = x ++ x // y is "olleHolleH"
y // final result is "olleHolleH"
```
we can replace `x` with `"Hello".reverse` everywhere, then reduce

```
val y : String = "Hello".reverse ++ "Hello".reverse 
// "olleHolleH"
```

we can see how the two snippets are equivalent,

val a  = it.next
 val b = a + 1  
 a + b

**Note on terminology**
> RT breakages are typically called "side-effects", which is an
unfortunate source of ambiguity.

> When advocates of pure FP say they
"Program without side-effects", they mean "without RT breakages", but
outsiders hear "without computational effects such as I/O", which
leaves them understandably confused.

>In this series we will always
prefer "programs as values" to "pure FP", and "RT breakage" to "side
effects".

## Execution as evaluation

define execution and evaluation
Strict language, defines a natural ordering of execution. Therefore execution is linked to evaluation, and to avoid execution we need to suspend evaluation explicitly.
Using the language we have developed, execution as evaluation is `doing` first, with explicit `being`. In scala, doing/being correspond closely to `val/:A` and `def/ : => A`. expand a bit on this: val a ; val b means do this then that, and :A means execute an action and pass its result. `def` instead means giving things a name.

Insert the content of appendix here? just mention that there are some limitations



## Programs as values

it's being first, with explicit doing
being means values, values means RT expressions
the easiest is data types
effects are represented as datatypes, which describe the intention of executing an effect
because they are values, `being` based (compositional) apis are very easy, just pass these data types around, or combine them with functions, or put them in data structures.
but how do we represent `doing`? instead of relying on evaluation, we design  combinator functions that describe the intention of running a program after another, i.e. we represent `doing` explicitly through a `being` api.
In the end, our whole program is represented by a single expression, which builds an instance of a datatype. This datatype can then be translated into actual computational effects, _once_, at the so called "end of the world". In Scala, this means the `main` function, Haskell goes one step further, and the translation is done in the runtime system, the programmer doesn't even see it.
This snippet of pseudo code should give you an idea of how things look like in a programs-as-values codebase

```scala
// the type of our program
type Console

// constructs Console values
def print(s: String): Console 

// simply returns a value of type console, does not do anything
val hey: Console = print("hey ")

// nothing gets printed, but we explicitly describe printing one word after another
val p: Console = hey.andThenDo(hey).andThenDo(print("you")) 

// Everything is RT, so p is equivalent to
val p: Console = hey.print("hey ").andThenDo(print("hey ")).andThenDo(print("you")) 

object Main extends App {
  def main(args: List[String]): Unit =
    p.translateToActualEffects
}
```

and believe or not, that's the whole idea: we represent effects as
datatypes (i.e. _programs as values_), and we use functions to assemble
big programs out of smaller ones. The rest is just about discovering
richer datatypes for our effects, and exploring. the structure of the
functions we use to combine them. And that is exactly what we are going to do next time.



### Appendix

=> A is not its own type (problem both as an input or as an output)
can solve both by using () => A (which is what => A translates to), but then you're limited
to sync things only.
The problem is solved by mixing in abstraction which are inspired by programs as values (like Future), but the resulting hybrid model becomes pretty hard to reason about as some effects happen via explicit combinators, other via execution
also, allowing effects in top-level `val`, inside `object` etc loses the property of having the whole program being a single expression, and having shared but not global state (link to talk).
Is it possible to imagine a language that uses execution as evaluation, without the drawbacks?
The answer is yes, algebraic effects, link to Unison and Frank paper, saying that that's where doing and being are introduced.



<!-- Generally, we tend to associate `being` with non-effectful things -->
<!-- like `5` or `"hello"`, but applying this principle to effectful -->
<!-- actions like `readLine` enables **compositional APIs** , such as -->
<!-- `repeatAction`. -->




<!-- move closer to our stated goal: compositionality. Note -->
<!-- how compositional `repeatAction` is, and how it critically depends on -->


<!-- using -->
<!-- effectful actions as _values_. -->


<!-- I'll call these two modes of use `doing` and `being` respectively. -->
<!-- `doing` is in a sense the essence of effectful computation: executing -->
<!-- actions and using their results in subsequent actions. `being` instead -->
<!-- characterises passing actions to functions, giving them names, putting -->
<!-- them in data structures and so on. In other words, it describes using -->
<!-- effectful actions as _values_. -->

<!-- Values can be freely passed around, and specifically they can be -->
<!-- passed to functions that combine them with other values, which -->
<!-- achieves our stated goal: compositionality. `repeatAction` is a compositional api, and it's only possible wouldn't be possible without the ability to take `readLine` -->

<!-- that can be combined -->
<!-- with other actions to achieve our stated goal: compositionality. -->


<!-- which we can then explicitly combine with other actions, _compositionally_. -->

<!-- by passing them to other  -->

<!-- In other words, it characterises _values_,  -->

<!-- I'll call these two modes of use `doing` and `being` respectively. -->
<!-- `doing` is in a sense the essence of effectful computation: executing -->
<!-- actions and using their results in subsequent actions. `being` instead -->
<!-- characterises using things as _values_: passing them to functions, giving -->
<!-- them names, putting them in data structures and so on.  -->
<!-- It might not seem as essential at first, but it's key to building -->
<!-- compositional code, to build bigger programs as a combination of -->
<!-- smaller ones, we need to be able to manipulate them before they are -->
<!-- executed. -->

<!-- because it lets us manipulate programs before we -->
<!-- execute them, so that we can build bigger programs out of smaller -->
<!-- ones. -->
<!-- In the example, `repeatAction` is a compositional api, and it -->
<!-- relies on the fact that we can pass `readLine` to it -->

<!-- what doing is, obviously useful. -->
<!-- what being is, passing things around, assigning them to values, putting them in datastructures, -->
<!-- less clear why it's useful, but it's key to building compositional apis -->


<!-- - doing and being in scala -->
<!-- put execution vs evaluation somewhere -->
<!-- def vs val, A, => A, little analysis? -->
<!-- => A is too transparent at call site, () => A too heavy at definition site -->
<!-- more importantly, cannot do async effects -->
<!-- cannot limit where effects happn -->

<!-- - What is a value -->
<!-- before seeing what programs as values are, formalise the concept of being -->
<!-- referential transparency, rt breakages, note on terminology -->

<!-- - Programs as values: doing through being -->


<!-- Final note: -->
<!-- cite Frank, and mention that it's possible to think of languages without programs as values, similar to the scala section but without the limitations -->

<!-- doing: sequencing being a value: referential transparency -->

<!-- core idea: being only, unlink evaluation, data, explicit combination -->


<!-- algebraic structure: intro form, combinators, elimination forms -->

Algebras comes from universal algebra
An algebra is a set (called the carrier) with a finite set of total functions that have the carrier set as their common codomain.

A Sigma algebra (signature algebra) defines the set of typed operator symbols without specifyingfunctions that would be the actual operators. Thus a signature defines a class of algebras, those whose operators conform to the typing contraints (used for typeclasses, ML modules, abstract data type etc)




<!-- progression: monoid, why F[A], functor, (split here?) monad, -->
<!-- monaderror -->



<!-- --- -->
