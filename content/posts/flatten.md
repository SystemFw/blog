---
title: A case study of flatMap vs flatten
date: 2019-07-11
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
`flatMap` is not not safe in the presence of concurrency: a concurrent
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

final session on the nature of flatMap
flatMap runs two programs one after the other, when the structure of the second depends on the resu
lt of the first (A =>), but if you take apart, map runs the first program and uses the result to decide how the second should look like, and `flatten` actually runs the resulting inner program. More often than not this two things are useful together, but sometimes you might want to decide which program to run differently (like in `modify` with transactionality), or run other operations first (like in `orElse`). Monads are about substitution followed by renormalisation, and sometimes it's useful to manipulate the denormalised F[F[A]] on its own.
Hope you enjoyed it.

