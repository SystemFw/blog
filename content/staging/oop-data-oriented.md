---
title: "OOP vs Data-oriented design...in a distributed FP language"
date: 2025-04-03
---

It is often said that "Learning _x_ will make you a better
programmer", for various values of _x_.

In my experience the secret to making that true lies not so much in
the choice of a given _x_, but rather in the ability to apply ideas
from _x_ to _y_ , and then exposing oneself to a wide range of
ideas.

It is challenging, however, to show a fully worked example of this
process, which by its very nature is not self contained.

Well, this post aims to do exactly that. Let's dive in.

TODO if I split it in parts, describe them


## Intro

My day job involves designing and implementing distributed systems for
[Unison Cloud](unison.cloud), the next-gen cloud platform we're
building on top of the [Unison](unison-lang.org) language.

A unique characteristic of our cloud is the power to manipulate
persistent and transactional storage as if it was an in-memory
datastructure.

We expose this feature as a set of _abilities_ (Unison's take on
algebraic effects) which let us express custom control flow
abstractions as ordinary straight-line code.

The model is fairly simple: you write programs in the `Transaction`
ability that operate on one or more typed key-value tables, and call
`transact` to delimit a transactional boundary.

```haskell
type Table k v = Table Text

ability Transaction where
  write.tx : Table k v -> k -> v ->{Transaction} ()
  tryRead.tx : Table k v -> k ->{Transaction} Optional v
  delete.tx : Table k v -> k ->{Transaction} ()
  
transact : Database -> '{Transaction, Exception, Random, Batch} a ->{Exception, Storage} a
```

As a quick syntax primer, `a -> b ->{g} c` is a function from `a` and
`b` to `c` that performs effects defined by the `g` ability, `'{g} a`
is syntactic sugar for the type of the thunk `() ->{g} a`, and lower
case letters in type signatures indicate generic type parameters.
Function calls are just whitespace (`f a b`), `do ...` is syntactic
sugar for thunks `_ -> ...`, and `|>` creates function pipelines.

Here's the canonical bank transfer example, Bob sends 10 pounds
(represented as pennies) to Alice:

```haskell
-- populated with data elsewhere
accounts: Table UserId Nat
accounts = Table "accounts"

transfer: Database ->{Exception, Storage} ()
transfer database = 
  bob = UserId "Bob"
  alice = UserId "Alice"
  transact database do
    from = read.tx accounts bob
    to = read.tx accounts alice
    amount = 10 * 100
    if balance >= amount
    then 
      write.tx accounts bob (from - amount) 
      write.tx accounts alice (to + amount)
    else 
      Exception.raiseGeneric "insufficient balance" bob

-- no infra needed to run code on cloud
Cloud.run do
  db = Database.default()
  Cloud.submit Environment.default() do transfer db
```

The code snippet above runs `transfer` on Unison Cloud, where the data
is persisted on our distributed storage, and the implementation of
`transact` guarantees that the transaction executes atomically.

## Persistent data structures

Transactions on typed key-value tables are a flexible building block,
and we can build data structures just like we would for in-memory
data.

Let's build a `Counter`:
```haskell
type Counter = Counter (Table () Nat)

Counter.named: Text -> Counter
Counter.named name = Counter db (Table name)
```

The idea is that we can represent the state of the counter in a table
with a single key of the unit type `()` . Just like any other other
data structure, we can write functions that operate on our `Counter`,
using the `Transaction` ability to retain atomicity:

```haskell
Counter.getAndIncrement: Counter ->{Transaction} Nat
Counter.getAndIncrement counter =
  (Counter state) = counter
  n = tryRead.tx state () |> Optional.getOrElse 0
  write.tx state () (n + 1)
  n
```

We can use our `Counter` to build an append-only log, similar to a
Kafka partition, by pairing a `Counter` with `Table Nat a`. The
`Counter` will keep track of the current size of the log so we know where
to append next, and the table will host the elements, indexed by their
offset.

```haskell
type Log a = Log Counter (Table Nat a)

Log.named: Text -> Log a
Log.named name = Log (Counter.named (name ++ "-size")) (Table name)
```

Reading an element from the log is straightforward:

```haskell
Log.at.tx : Nat -> Log a ->{Transaction} Optional a
Log.at.tx n log =
  (Log _ elems) = log
  tryRead.tx elems n
```

Appending to the log is slightly more involved but still pretty easy:

```haskell
Log.append.tx: a -> Log a ->{Transaction} ()
Log.append.tx v log =
  (Log size elems) = log
  n = incrementAndGet size
  write.tx elems n v
```

note that `size` and `elems` cannot go out of sync since we're still
in the `Transaction` ability.

A call to `append.tx` executes atomically when we call `transact`:

```haskell
type Track = Track Text

playlist: Log 
playlist = Log.named "my-playlist"

transact myDb do
  myLog |> append.tx (Track "Obstacles")
```

and we can append multiple elements atomically as well:

```haskell
transact myDb do
  myLog |> append.tx (Track "Sea above, sky below")
  myLog |> append.tx (Track "Featherweight")
```

Let's conclude this section with a couple of notes on semantics that
will be useful later.

The first is that the `Table` constructor, and by extension
`Counter.named` and `Log.named`, don't actually create anything in
storage there and then. A `Table` is just a logical handle, all the
action happens during `transact`, which takes the actual `Database` we
will modify. This is actually guaranteed by the types too:
`Counter.named` and `Log.named` don't advertise any abilities, which
means that they have no side-effects.


semantics: named is pure, read/write perf













