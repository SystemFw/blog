---
title: A case study of flatMap vs flatten
date: 2019-07-24
---

The correspondence between `flatMap` and `map + flatten` is well
known, and in practical use `flatMap` is much more common, but what
are some of the use cases for using `flatten` on its own?  
We are going to look at two examples, using **Typelevel** libraries
for our case study, and in the end we'll hopefully discover
something interesting about the nature of `flatMap`.

**Note**: this post is very much not a monad tutorial, you are
expected to be already familiar with monads in practical use.


## Definitions recap

Let's forget the existence of `Applicative` for this post, and look at
these two definitions of `Monad`:

```scala
trait Monad[F[_]] {
  def pure[A](a: A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}
```

```scala
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}
trait Monad[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]
  def flatten[A](ffa: F[F[A]]): F[A]
}
```

which are, of course, equivalent:

```scala
// definition 2) in terms of 1)
def map[A, B](fa: F[A])(f: A => B): F[B] =
  flatMap(fa)(a => pure(f(a)))
def flatten[A](ffa: F[F[A]]): F[A] =
  flatMap(identity)

// definition 1) in terms of 2)
def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
  flatten(map(fa)(f))
```

Now, definition 2) in terms of `map` and `flatten` (also called
`join`) is closer to the Category Theory definition of Monad, and
therefore often preferred for theoretical explanations, but the goal
of this post is different: we are going to focus on a couple of
_practical_ use cases where it makes sense to use `flatten` on its
own, instead of `flatMap.`


## Example 1: Ref.modify

For our first example, we are going to look at `cats.effect.Ref`, a
purely functional, concurrent mutable reference which I've already
described in a [couple](https://vimeo.com/294736344) of
[talks](https://www.youtube.com/watch?v=-dLp3u6y2DQ).
You can check them out if you aren't familiar with it, but for now we
will have a look at this slightly simplified API:

```scala
import cats.implicits._
import cats.effect.IO

trait Ref[A] {
  def get: IO[A]
  def set(a: A): IO[Unit]
  def modify[B](f: A => (A, B)): IO[B]
}
```

`get` and `set` should be self explanatory, so let's look at an
example of `modify`: imagine we're modeling a race, and each sprinter
needs to update their arrival position and return it.

```scala
def buggySprinter(finishLine: Ref[Int]): IO[Int] =
  finishLine.get
    .flatMap(pos => finishLine.set(pos + 1))
    .flatMap(_ => finishLine.get)
```

This may seem correct at first glance, but note how the first
`flatMap` is not safe in the presence of concurrency: a concurrent
process could change the value of the `Ref` in between `get` and `set`
(resulting in lost updates) , so we need an atomic `update(f: A => A):
IO[Unit]`.

This is not enough either though: a concurrent process could change
the value after we update it but before we get it (resulting in an
incorrect final result), so we also need the ability to return a value
atomically, at which point we have rediscovered `modify`:

```scala
def sprinter(finishLine: Ref[Int]): IO[Int] =
  finishLine.modify { pos =>
    (pos + 1, pos + 1)
  }
```

### Modify, flatten and transactionality

Depending on your concurrency background, you might be thinking of
`modify` as involving locks, but the way things actually
work is closer to transactions: the update is retried on concurrent
conflicts until it succeeds, using something called a **CAS loop**.
Under this perspective, we can see why the `flatMap` based examples
were problematic: `flatMap` on `IO` has no concept of
transactionality, which is given by the special `modify` method.

To see where `flatten` comes into place, I'm going to very briefly
introduce a powerful pattern involving `Ref`: concurrent state
machines (again, watch
[this](https://www.youtube.com/watch?v=-dLp3u6y2DQ) if you want to
know more).  
In a concurrent state machine, there is a set of _states_,
_transitions_ between these states, and _actions_ to be run upon each
transition. Moreover, the transitions are triggered concurrently,
meaning that the transition function needs to follow the transactional
pattern outlined above: only when the concurrent transition is
successfully registered the corresponding action needs to run.

For example, let's imagine a simple machine which will notify
the user via an Http call when the lights are switched on or off.

```scala
// this is the set of states
// it's going to be held in a `Ref`
sealed trait LightSwitch
case object On extends LightSwitch
case object Off extends LightSwitch

// imagine something richer
type HttpStatus = Int

// these are our actions
trait Notifications {
  // e.g. via http call
  def notify(msg: String): IO[HttpStatus]
}

// our finite state machine
trait FSM {
  def toggle: IO[HttpStatus]
}
```

So, how do we implement `toggle`? Remember that `Ref.modify`
allows us to atomically change the state and return a value, and `IO`s
are values, which means we can return _the action itself_.

```scala
modify(f: A => (A, B): IO[B]
modify(f: DoorState => (DoorState, IO[HttpStatus])): IO[IO[HttpStatus]]
```


It's important to understand the semantics of that `IO[IO[HttpStatus]]`:
when the outer `IO` runs, it will try to change the current state of `FSM`,
possibly retrying multiple times in case of conflict, but _without_ running the inner
`IO[HttpStatus]`, which is merely returned at the end.  
Once the outer `IO` returns, it means that the state of `FSM` has been
successfully changed, and it is now possible to run the action
represented by the inner `IO[HttpStatus]`. In other words, we now need
to go from `IO[IO[HttpStatus]]` to `IO[HttpStatus]`, which we can do
with... `flatten`.  
The full code looks like this:

```scala
def fsm(states: Ref[LightSwitch], actions: Notifications): FSM = new FSM {
  def toggle: IO[HttpStatus] = states.modify {
    case Off => (On, actions.notify("The light has been turned on"))
    case On => (Off, actions.notify("The light has been turned off"))
  }.flatten 
}
```

------

So we have our first case of standalone `flatten`, which happens when
there is some notion of _transactionality_ : you can think of
`F[F[A]]` as _a program that returns another program_, where the first
program is transactional and the second is not.

We used `Ref.modify`, but this principle extends to other scenarios as well:

- The [Doobie](https://tpolecat.github.io/doobie/) library exposes a
  `ConnectionIO[A]` type that represents a database transaction. So
  you would have a `ConnectionIO[IO[A]]`, `transact` to get to
  `IO[IO[A]]`, then `flatten`.
- Haskell's [Software Transactional Memory](https://haskell.fpcomplete.com/library/stm) is based on an `STM a`
  type that represents a transactional concurrent computation
  (similar to a multivariable `Ref`). So you would have an `STM (IO
  a)`, `atomically` to get to `IO (IO a)`, then `join` (Haskell's
  version of `flatten`).

## Example 2: JSON decoding

For our second example we are going to look at JSON decoding with
[Circe](https://circe.github.io/circe/), focusing on a specific issue my
team encountered recently.
We have some simple JSON, representing our two different types of users:

```scala
import io.circe._
import io.circe.parser._
import io.circe.generic.semiauto._

def json(s: String): Json = parse(s).getOrElse(Json.Null)

def named = json {
 """
  {
    "named" : {
      "name": "Dotty",
      "surname" : "McDotFace"
   }
  }
 """
}

def unnamed = json {
 """
  {
   "unnamed" : { 
     "id" : 13355
   }
  }
 """
}
```

which maps to the following ADT:

```scala
sealed trait User
case class Unnamed(id: Long) extends User
case class Named(name: String, surname: String) extends User
```

So let's go ahead and write a `Decoder` for it

```scala
def unnamedDec: Decoder[User] = 
  deriveDecoder[Unnamed]
   .prepare(_.downField("unnamed"))
   .widen[User]  

def namedDec: Decoder[User] = 
  deriveDecoder[Named]
   .prepare(_.downField("named"))
   .widen[User]

def userDec = unnamedDec orElse namedDec

userDec.decodeJson(named)
// res2: Decoder.Result[User] = Right(Named("Dotty", "McDotFace"))
userDec.decodeJson(unnamed)
// res3: Decoder.Result[User] = Right(Unnamed(13355L))
```

A few points about the code above:

- `deriveDecoder` automatically derives a decoder for the individual cases.
- `prepare` modifies the input json before feeding it to the
  decoder. In our case we need to access the "unnamed" and "named"
  json objects before decoding the corresponding data.
- `widen` changes a `Decoder[B]` into a `Decoder[A]` when `B <: A`. It's the explicit equivalent of covariance.
- `decA orElse decB` will try `decA`, and fallback on `decB` if `decA` fails.

So far this works great, but look at what happens when we send an incorrect unnamed user:

```scala
def incorrectUnnamed: Json = json {
    """
    {
      "unnamed" : {
        "id" : "not a long"
      }
    }
    """
  }

userDec.decodeJson(incorrectUnnamed)
// res4: Decoder.Result[User] = Left(
//   DecodingFailure(Attempt to decode value on failed cursor, List(DownField(named)))
// )
```

As you can see, `Circe` does give back a nice error message, but
unfortunately it's coming from the wrong branch.
We know that if the json contains `unnamed`, it's an unnamed user, but
circe does not: it sees a failure and falls back to the named user
decoder with `orElse`, which obviously fails to decode as well, at
which point you get the error from the last branch.  
In general, this is ok, but in our specific use case it was a source
of pain for the users of our API, so we wanted to report errors
pertinent to the ADT case they were sending to us (assuming they got
at least the "named/unnamed" tag right).

### flatMap and orElse

To see where the problem lies, it's easy to think of the code above as
being made of these four components (not 100% true in `Circe` terms,
but the differences are irrelevant):

- an "unnamed" accessor `Decoder`, of type `unnamedField: Decoder[Json]`
- a `Decoder` for unnamed users, of type `unnamedData: Json => Decoder[User]`
- a "named" accessor `Decoder`, of type `namedField: Decoder[Json]`
- a `Decoder` for named users, of type `namedData: Json => Decoder[User]`

where the whole decoder is

```scala
unnamedField.flatMap(unnamedData) orElse namedField.flatMap(namedData)
```

if you look for example at `unnamedField.flatMap(unnamedData)`, you
can see that there are two possible sources of error; one is in
`unnamedField`, which means the tag is not "unnamed", and one is in
`unnamedData`, which means that the data format is wrong.  
Crucially, we want `orElse` to only operate on the first source, so we
have to separate them. One way to do that would be:

```scala
unnamedField.orElse(namedField).flatMap { json =>
   unnamedData(json) orElse namedData(json)
}
```

But that's not ideal: first of all there is repetition of `orElse` (in
our actual scenario there were many more cases), but also it's
actually weird to have to define `unnamedField` and `unnamedData`
separately.  
We somehow want to keep them together, but without triggering the
second source of errors until _after_ `orElse`, or in other words, we
want to _return_ the second `Decoder` _program_  without _running it_
(and therefore triggering its errors).  
Again, this corresponds to `Decoder[Decoder[User]]`, which we can get
by changing `flatMap` to `map`:

```scala
unnamedField.map(unnamedData) orElse namedField.map(namedData)
```

`orElse` will give us another `Decoder[Decoder[User]]`, and we can now
run the inner decoder with... `flatten`.  
And this is our second case of standalone `flatten`, which happens
when we want *to interleave another operation* (in this case `orElse`)
in between the `map` and `flatten` parts of `flatMap`.

--------

The full code contains some `Circe` details which aren't super
relevant conceptually, but I'm leaving it here for the interested
readers. Note the correct, informative error trail at the end.

```scala
implicit class Tagger[A](d: Decoder[A]) {
  def tag(accessor: String): Decoder[Decoder[A]] = Decoder.instance { inputJson =>
    inputJson.downField(accessor) match {
       case innerJson: HCursor => Right(innerJson)
       case err: FailedCursor => Left(DecodingFailure("Failed cursor", err.history))
     }
   }.map(outJson => Decoder.instance(_ => d(outJson)))
}

def betterDec: Decoder[User] =
  deriveDecoder[Unnamed]
   .widen[User]
   .tag("unnamed")
   .orElse {
      deriveDecoder[Named]
       .widen[User]
       .tag("named")
   }.flatten

betterDec.decodeJson(named)
// res5: Decoder.Result[User] = Right(Named("Dotty", "McDotFace"))
betterDec.decodeJson(unnamed)
// res6: Decoder.Result[User] = Right(Unnamed(13355L))
betterDec.decodeJson(incorrectUnnamed)
// res7: Decoder.Result[User] = Left(
//   DecodingFailure(Long, List(DownField(id), DownField(unnamed)))
// )
```

## Conclusion: the nature of `flatMap`

Let's look again at the signature of `flatMap`:

```scala
def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
```

you can look at `F[A]` as a program that returns `A`s, and at `flatMap`
as a mode of composition that represents running two programs in
sequence, using the result of the first to decide the shape of the
second (`A => F[B]` means exactly "`F[B]` depends on `A`").  
But as we've seen, there are actually two components at play, `map`
and `flatten`, so we can now refine our intuition: `map` represents
running the first program and using its result to decide the shape of
the second, and `flatten` then actually runs the second program.  
Most of the time, these two things happen as a unit (hence why
`flatMap` is more prominent), but not all the time, either because we
want to take the decision about which program to run in a different
way (in the case of `modify`, transactionally), or because we need to
run other operations first (like `orElse`).  
Slightly more formally, monads are about substitution (`map`) followed
by renormalisation (`flatten`) and, as we've seen, sometimes you
need to manipulate the non normalised form `F[F[A]]` on its own.

Hope you enjoyed this post, and see you next time!

