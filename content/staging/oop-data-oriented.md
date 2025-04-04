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

## Intro: distributed transactions in Unison

My day job involves designing and implementing distributed systems for
[Unison Cloud](unison.cloud), the next-gen cloud platform we're
building on top of the [Unison](unison-lang.org) language.

A unique characteristic of our cloud is the power to manipulate
persistent and transactional storage as if it was an in-memory
data structure.

We expose this feature as a set of _abilities_ (Unison's take on
algebraic effects) which can express custom control flow
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

-- like tryRead.tx, but fails on key not found
read.tx: Table k v -> k ->{Transaction, Exception} v


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

-- no infra needed to run code on cloud!
Cloud.run do
  db = Database.default()
  Cloud.submit Environment.default() do -- supports the Storage ability
    transfer db
```

The code snippet above runs `transfer` on Unison Cloud, where the data
is persisted on our distributed storage, and the implementation of
`transact` guarantees that the transaction executes atomically.

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
Log.at : Nat -> Log a ->{Transaction} Optional a
Log.at n log =
  (Log _ elems) = log
  tryRead.tx elems n
```

Appending to the log is slightly more involved but still pretty easy:

```haskell
Log.append a -> Log a ->{Transaction} ()
Log.append v log =
  (Log size elems) = log
  n = getAndIncrement size
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
  myLog |> append (Track "Obstacles")
```

and we can append multiple elements atomically as well:

```haskell
transact myDb do
  myLog |> append (Track "Sea above, sky below")
  myLog |> append (Track "Featherweight")
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

The second is about the execution model of transactions, to help us
reason about performance. The implementation of `transact` uses
Optimistic Concurrency Control, which means that reads go to storage,
whilst writes are buffered in memory. When the transaction completes,
`transact` will try to atomically commit all the writes to storage at
once. If there is a conflict, it retries the whole transaction, which
it can do because the Unison type system guarantees that the thunk
passed to `transact` doesn't perform any other effects that wouldn't
be safe to retry arbitrarily (like an HTTP call).

## Case study: stream storage

We can now look at the problem this post will be centred around, which
is a simplified version of something I've encounted while developing
[volturno](https://share.unison-lang.org/@systemfw/volturno/code/main/latest),
a distributed stream processing framework.

Imagine we need to design the storage layer for keyed data streams.
For our toy version, we can go with something like this:

```haskell
type Key
type Event

streams: Table Key (Log Event)
```

Where we basically store a log of events for each key (the real code
uses sharding, but we will keep the simpler version above in this
post).

We want to implement a function to publish a batch of events to our
`streams` `Table`:

```haskell
publish: Database -> [(Key, Event)] ->{Remote} ()
```

`Remote` is the ambient ability in Unison Cloud, it's very powerful
but we won't cover it here for space reasons, just know that we can
call `toRemote` to embed other cloud abilities in it, like `Storage`
and `Exception` in our case.

Ok, let's plan the implementation out. We need to start grouping the
batch of events by key, which we can do with:

```haskell
List.groupMap : (a -> k) -> (a -> v) -> [a] -> Map k (List.Nonempty v)
```

We want to upload batches of events for different keys in parallel, 
which we can do with `Remote.parMap`.

```haskell
Remote.parMap : (a ->{Remote} t) -> [a] ->{Remote} [t]
```

At that point we will be dealing with batches of events for a single
key that have to be appended sequentially, so we can have a
transaction that fetches the relevant `Log` and iterates through the
keyed batch to append the events.

Here's the full code:

```
publish: Database -> [(Key, Event)] ->{Remote} ()
publish db messages = 
  messages
   |> List.groupMap at1 at2
   |> Map.toList
   |> Remote.parMap (messages ->
       toRemote do
         (key, events) = messages
         transact db do
           log = read.tx streams key
           events
            |> toList
            |> foreach_ (event -> log |> append event)
     )
   |> ignore
```


## Optimisation 
