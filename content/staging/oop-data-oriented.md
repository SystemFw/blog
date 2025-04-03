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
Function calls are just whitespace (`f a b`), and `do ...` is
syntactic sugar for thunks `_ -> ...`.

Here's an example showing a bank transfer of 10 "pounds" ( horribly
represented as just a non-negative integer) between Alice and Bob.

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
    if balance >= 10 
    then 
      write.tx accounts bob (from - 10) 
      write.tx accounts alice (to + 10)
    else 
      Exception.raiseGeneric "insufficient balance" bob

-- no infra needed to run code on cloud
Cloud.run do
  db = Database.default()
  Cloud.submit Environment.default() do transfer db
```

The code snippet above runs `transfer` on Unison Cloud, where the data
is persisted on our distributed storage, and the implementation of
`transact` guarantees that the funds are transferred atomically.

## Persistent data structures

Typed table and transactions are powerful building blocks, and we can
build data structures just like we would for in-memory data.

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

Equipped with our counter, add Log
then move to the real example with publish 
  Table Key (Log Event)
  [(Key, Event)]
remark that the final version of the code is already ugly, but in the real code is terrible
I'm afraid I'll have to show read as well though.













