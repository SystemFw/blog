---
title: "OOP vs Data oriented design...in a distributed FP language"
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
type UserId = UserId Text

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
Counter.named name = Counter (Table name)
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

A call to `Log.append` executes atomically when we call `transact`:

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
a distributed stream processing framework (think Kafka + Flink).

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
call `toRemote` to embed other cloud abilities in it, like `Storage`,
`Exception` or `Random`.

Ok, let's plan the implementation out. We need to start grouping the
batch of events by key, which we can do with:

```haskell
List.groupMap : (a -> k) -> (a -> v) -> [a] -> Map k (List.Nonempty v)
```

We then want to upload batches of events for different keys in parallel, 
let's use `Remote.parMap`:

```haskell
Remote.parMap : (a ->{Remote} t) -> [a] ->{Remote} [t]
```

At this point we will be dealing with batches of events for a single
key that have to be appended sequentially, so we can have a
transaction that fetches the relevant `Log` and iterates through the
keyed batch to append the events.

Here's the full code. Note the use of the `cases` keyword to pattern
match on the `(key, events)` tuple in a lambda:

```haskell
publish: Database -> [(Key, Event)] ->{Remote} ()
publish db messages = 
  messages
    |> List.groupMap at1 at2
    |> Map.toList
    |> Remote.parMap cases (key, events) ->
         toRemote do
           transact db do
             log = read.tx streams key
             events
               |> toList
               |> foreach_ (event -> log |> append event)
    |> ignore
```

That looks pretty good, but unfortunately it has a bug. We fetch the
relevant log with `read.tx streams key`, which will fail if the log
isn't there, _but nothing guarantees the log will be there_. The logs
are per key, so we cannot create them all in advance as we don't know
all the keys in advance. Instead, we will create each log on demand if
we cannot find it in storage when we want to write some messages to
it. We will use `randomName: '{Random} Text` to generate a name for
our log:

```haskell
publish: Database -> [(Key, Event)] ->{Remote} ()
publish db messages = 
  messages
    |> List.groupMap at1 at2
    |> Map.toList
    |> Remote.parMap cases (key, events) ->
         toRemote do
           transact db do
             log = match tryRead.tx streams key with
               Some log -> log
               None ->
                 log = Log.named randomName()
                 write.tx streams key log
                 log
             events
               |> toList
               |> foreach_ (event -> log |> append event)
    |> ignore
```


## Wrestling with optimisation 

We're dealing with a performance sensitive system, so we have to be
conscious about optimising our code properly. To start with, we're
publishing all the events for a given key in a single transaction.
Transactions actually have a size limit, so this isn't wise.
On the other hand, we don't want to publish each event in its own transaction
and give up batching entirely, so we'll compromise by sending events in
batches of 25, using `chunk: Nat -> [a] -> [[a]]` for help:


```haskell
publish: Database -> [(Key, Event)] ->{Remote} ()
publish db messages = 
  messages
   |> List.groupMap at1 at2
   |> Map.toList
   |> Remote.parMap cases (key, events) ->
       toRemote do
         events
          |> chunk 25
          |> foreach_ (chunk ->
               transact db do
                 log = match tryRead.tx streams key with
                   Some log -> log
                   None ->
                     log = Log.named randomName()
                     write.tx streams key log
                     log
                 events
                   |> toList
                   |> foreach_ (event -> log |> append event)
             )
   |> ignore
```

Ok, but now note how `tryRead.tx` is read from storage multiple times
(one per chunk), even though we know that after the first chunk it
will certainly have been created (by us) if it didn't exist.

Well, we can move the code that checks or creates the log to a separate
transaction at the start:

```haskell
publish: Database -> [(Key, Event)] ->{Remote} ()
publish db messages = 
  messages
   |> List.groupMap at1 at2
   |> Map.toList
   |> Remote.parMap cases (key, events) ->
       toRemote do
         log = transact db do
           match tryRead.tx streams key with
             Some log -> log
             None ->
               log = Log.named randomName()
               write.tx streams key log
               log
         events
          |> chunk 25
          |> foreach_ (chunk ->
               transact db do
                 events
                   |> toList
                   |> foreach_ (event -> log |> append event)
             )
   |> ignore
```

Remember that the above is still correct: even if we get the `log` and
something else modifies it straight after, each call to `Log.append`
is checking the size of the log transactionally before appending
anyway.

This version of the code does avoid reading the log on each chunk,
but it's still not optimal: if the log doesn't exist and we do need to
create it, we will have this extra transaction just to create the log,
instead of creating it in the same transaction that also adds the
first chunk. In other words, we're wasting one transactional roundtrip
which could carry some messages instead.

The optimal behaviour requires trickier code, we want to get (and
potentially create) the log on the first chunk, and then carry it
across the other chunks afterwards, using a fold:

```haskell
publish: Database -> [(Key, Event)] ->{Remote} ()
publish db messages = 
  messages
   |> List.groupMap at1 at2
   |> Map.toList
   |> Remote.parMap cases (key, events) ->
       toRemote do
         events
          |> chunk 25
          |> foldLeft_ None (log chunk ->
               transact db do
                 log' = match log with
                   Some log -> log
                   None -> match tryRead.tx streams key with
                     Some log -> log
                     None ->
                       log = Log.named randomName()
                       write.tx streams key log
                       log
                 events
                     |> toList
                     |> foreach_ (event -> log' |> append event)
                 log'
             )
   |> ignore
```

Ok, this behaves as we want it to... but it's pretty gnarly. It's not
_terrible_ in this short snippet, but the real code dealt with
additional concerns such as error handling, and this log creation
logic was really tipping the scale and making it hard to understand.

Now, it's reasonable at this point to want to introduce some named
helpers to clean it up, but that's not as great an idea as it sounds
in this type of system: named helpers might preserve (or even clarify)
the _intent_ of the code, but they obscure the access patterns to the
data, which is important information for systems code to convey. 
A couple of named helpers (`mapChunked`, `getLog`) can make the very
first snippet in this section look quite harmless, for example:

```haskell
events |> mapChunked (chunk ->
  transact db do
    log = getLog key
    events
      |> toList
      |> foreach_ (event -> log |> append event)
)
```

On top of that, there's another instinct that's even more pernicious:
a subtle bias to make the behaviour of the system worse in order to
have prettier code. Not every optimisation is worth its complexity, of
course, however pretty code is first and foremost a tool to achieve
the desired behaviour, not the other way around. This risk is
particularly prominent in a functional programming language, which is
typically geared towards elegant code.

So, is this the best we can do? Turns out ... segue into DoD/OOP

TODO maybe add a helper for the per-key publish , and then just use
that to write the various examples instead of repeating the full code
`publishKey`, "there is a bug in `publishKey`", need to see if it's
still worth introducing `cases` (default yes)


I think just go "unison is not an OO language" and start directly with
the comparison, with the no GC thought experiment, and then talk about
the Data Oriented design philosophy, instead of a completely
standalone section on OO vs DoD which requires extra examples
