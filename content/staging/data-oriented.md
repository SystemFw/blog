---
title: "OOP vs Data oriented design...in a distributed FP language"
date: 2025-04-03
---

It is often said that "Learning _x_ will make you a better
programmer", for various values of _x_.

In my experience the secret to making that true lies not so much in
the choice of a given _x_, but rather in the ability to apply ideas
from _x_ to _y_ , and then exposing oneself to a wide range of
such ideas.

It is challenging, however, to show a fully worked example of this
process, which is not self contained by its very nature.

Well, this post aims to do exactly that. Let's dive in. 

TODO: maybe add a tiny summary to entice the reader a bit more, like
"exactly that. We'll start here, then go there, then finish etc. Let's
dive in"

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


transact : Database -> '{Transaction, Exception, Random} a ->{Exception, Storage} a
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
transfer db = 
  bob = UserId "Bob"
  alice = UserId "Alice"
  transact db do
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

### Data structures

Transactions on typed key-value tables are a flexible building block,
and we can build data structures like we do for in-memory data.

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

where we basically store a log of events for each key (the real code
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

Ok, let's plan the implementation out. We need to start by grouping
the batch of events by key, which we can do with:

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
         toRemote do publishKey db key (toList events)
    |> ignore
    
publishKey: Database -> Key -> [Event] ->{Storage, Exception} ()
publishKey db key events =
  transact db do
    log = read.tx streams key
    events |> foreach_ (event -> log |> append event)
```

That looks pretty good, but unfortunately `publishKey` has a bug. We
fetch the relevant log with `read.tx streams key`, which will fail if
the log isn't there, _but nothing guarantees the log will be there_.
The logs are per key, so we cannot create them all in advance as we
don't know all the keys in advance. Instead, we will create each log
on demand if we cannot find it in storage when we want to write some
events to it. We will use `randomName: '{Random} Text` to generate a
name for our log:

```haskell
publishKey: Database -> Key -> [Event] ->{Storage, Exception} ()
publishKey db key events =
  transact db do
    log = match tryRead.tx streams key with
      Some log -> log
      None ->
        log = Log.named randomName()
        write.tx streams key log
        log
    events |> foreach_ (event -> log |> append event)
```


### Wrestling with optimisation 

We're dealing with a performance sensitive system, so we have to be
conscious about optimising our code properly. To start with, we're
publishing all the events for a given key in a single transaction.
Transactions actually have a size limit, so this isn't wise.
On the other hand, we don't want to publish each event in its own transaction
and give up batching entirely, so we'll compromise by sending events in
batches of 25, using `chunk: Nat -> [a] -> [[a]]` for help:

```haskell
publishKey: Database -> Key -> [Event] ->{Storage, Exception} ()
publishKey db key events =
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
           chunk |> foreach_ (event -> log |> append event)
       )
```

Ok, but now note how `log` is read from storage multiple times
(one per chunk), even though we know that after the first chunk it
will certainly have been created (by us) if it didn't exist.

Well, we can move the code that checks or creates the log to a separate
transaction at the start:

```haskell
publishKey: Database -> Key -> [Event] ->{Storage, Exception} ()
publishKey db key events =
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
           chunk |> foreach_ (event -> log |> append event)
       )
```

Remember that the above is still correct: even if we get the `log` and
something else modifies it straight after, each call to `Log.append`
is checking the size of the log transactionally before appending.

This version of the code does avoid reading the log on each chunk,
but it's still not optimal: if the log doesn't exist and we do need to
create it, we will have this extra transaction just to create the log,
instead of creating it in the same transaction that also adds the
first chunk. In other words, we're "wasting" one transactional
roundtrip that could carry some messages instead.

The optimal behaviour requires trickier code, we want to get (and
potentially create) the log on the first chunk, and then carry it
across the other chunks afterwards, using a fold:

```haskell
publishKey: Database -> Key -> [Event] ->{Storage, Exception} ()
publishKey db key events =
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
           chunk |> foreach_ (event -> log' |> append event)
           log'
       )
    |> ignore
```

Ok, this behaves as we want it to... but it's starting to get pretty
gnarly. It's not _terrible_ in this short snippet, but the real code
dealt with additional concerns such as error handling, and this log
creation logic was really tipping the scale and making it hard to
understand.

Now, it's reasonable at this point to want to introduce some more
named helpers to clean it up, but that's not as great an idea as it
sounds in this type of system: named helpers might preserve (or even
clarify) the _intent_ of the code, but they obscure the access
patterns to the data, which is important information for systems code
to convey. A couple of named helpers (`mapChunked`, `getLog`) can make
the very first version we had look quite harmless, for example:

```haskell
publishKey: Database -> Key -> [Event] ->{Storage, Exception} ()
publishKey db key events =
  events |> mapChunked (chunk ->
    transact db do
      log = getLog key
      events |> foreach_ (event -> log |> append event)
  )
```

On top of that, there's another instinct that's even more pernicious:
a subtle bias to make the behaviour of the system worse in order to
have prettier code. Not every optimisation is worth its complexity, of
course, however pretty code is first and foremost a tool to achieve
the desired behaviour, not the other way around. This risk is
particularly prominent in a functional programming language, which is
typically geared towards elegant code.

So, is this the best we can do? Turns out it's not, but to see how we
need to go looking for ideas in some unexpected places...

## OOP and Data Oriented Design

Unison is decidedly not an object oriented language: it has no
classes, no methods, no inheritance, not even interfaces. And yet, the
design we've seen embodies some of the ideas that have made OO
popular: it's based on data types that mirror their logical domain,
keep their internals encapsulated, and expose their behaviour through
an explicit api.

In fact, here's how our data model could be written in a
generic-looking OO language:

```scala
class Counter(state: Int) {
  def getAndIncrement(): Int {
    res := state
    state = state + 1
    return res
  }
}

class Log[A](counter: Counter, elems: Array[A]) {
  def at(n: Int): Int {
     return elems[n]
  }
   
  def append(a: A) {
    n = counter.getAndIncrement()
    elems[n] = a
  }
}

class Key(..)
class Event(..)

class Streams(streams: Map[Key, Log[Event]])
```

There is a lot to like about this model: the behaviour of each
component is easily understood just via its public api, and richer
behaviour is achieved by combining these small components together.

Furthermore, the common FP criticism about the dangers of mutation
wouldn't translate to our original code: transactions guarantee
serializability, which is very easy to reason about.

There is however another angle for critiquing this model, using the
perspective of Data Oriented Design. The ideas behind Data Oriented
Design come largely from videogame development, in a context where
optimising memory access matters a lot, and you're managing memory
manually.

So let's apply this lens to the code snippet above. We're going to
assume a runtime representation similar to Java, Scala or Ruby, except
without a GC.

The main thing to note is how many pointers are involved at runtime
here: `Streams` has a pointer to a `Map`, which has pointers to
various instances of `Log`, which have pointers to instances of
`Counter`, and so on.

This is problematic for two reasons:

- From a performance point of view, accessing a piece of data involves
  several roundtrips to memory as we hop from pointer to pointer.
  Also, creating data requires a bunch of tiny individual heap
  allocations.
- From a simplicity point of view, manual memory management is error
  prone as each pointer has to have its memory deallocated
  individually, and in a specific order. E.g. freeing our `Streams`
  class involves iterating over the `Map` to free each `Log`, and
  freeing each `Log` involves freeing the `Counter` and iterating over
  the `Array` to free each `A`, etc.

Data Oriented Design on the other hand would forego nested pointers in
favour of structuring data as flat arrays indexed by integer IDs. We
could then access it efficiently by reading whole chunks of memory,
and allocate it and deallocate it bulk, without worrying about the
lifetimes of its individual pieces.

But beyond specific technical strategies, Data Oriented Design
advocates for a very different way to approach data modelling: we
should not strive for code to reflect the logical domain we're working
in, and we should instead frame programs as data transformations, and
then identity the simplest, most efficient way for the _machine_ to
perform the desired transformation.

Now, it's easy to dismiss all this as supremely irrelevant to us: we
_do_ have a GC, we enjoy it very much thank you, and we're in a far
higher level language anyway where this minutiae ought not to matter.

But let's zoom out a bit: it is true that in a higher level language
there's less emphasis about counting every single memory access,
however in our transactional code the pointer hopping mapped to reads
from storage, and we _did_ care about minimising those. We also didn't
have to deal with manual deallocation of memory, but the chief
complication in `publishKey` could indeed be framed as a problem with
_lifetimes_, specifically about having to create these `Log` instances
at the right time.
 
So let's try to apply Data Oriented Design to our problem and see if
it bears any fruit.

### Improve the code

TODO: fill this section (and improve title)

recap data access pattern, then describe data analysis, and new code

## Conclusion

You shouldn't walk away from this post thinking that there is a
universal winner between the two approaches we discussed today.
Instead, you should look critically at the tradeoffs. 

The OO design is more composable, it gives us nice building blocks
that we can package in a library and reuse in interesting ways to
easily assemble novel behaviour. Conversely, if we're willing to give
up flexibility, the data oriented design can result in simpler code,
and with an easier path to optimal performance.

There is something really deep here in my opinion, about the nature of
abstraction itself. Abstraction is the most powerful tool we have to
expand the ceiling of our skill when we don't fully grasp the
problem/solution space, and it also adapts gracefully if the problem
space changes. However, if we do understand that space, tearing the
abstraction apart reveals hidden details that can lead us to a more
optimal solution, albeit one that's more set in stone.

TODO: ^ this paragraph needs to be better. 

I would argue that a greater appreciation of these tradeoffs is as big
a payoff from our journey than the code improvement that we got. 

In fact, beyond any specific technique, it is the compounding of this
type of understanding that, in my experience, actually makes you a
better programmer.

## Appendix: 3 practical points for exploiting far out ideas

- A lot of interesting ideas are found in somewhat niche communities:
  functional programming, game development, formal methods, futuristic HCI, etc.
  Be curious.

- At a first glance, many bad ideas make perfect sense, and many good
  ideas sound like utter nonsense. Don't take everything at face
  value, but don't close your mind too early either.

- Unfortunately, cool ideas are often presented as universal truths
  that ought to apply to every field of software engineering. Instead,
  they are typically borne out of a specific context. Strive to really
  understand it, and you will find ways to apply those ideas in
  interesting, unexpected places that share just a tiny bit of that
  context. Equally as importantly, you will know when you can safely
  dismiss them as well.


TODO Data Oriented Design and abstraction resolution or, Learning _ might make you a better programmer
