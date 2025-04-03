---
title: "OOP vs Data-oriented design...in a distributed FP language"
date: 2025-04-03
---

I enjoy drawing connections between seemingly distant areas of
Software Engineering, and I've noted in several one-on-one
conversations how much value I get from this strategy.

Outside the context of a long conversation, however, is challenging to
show a fully worked example of this process, which by its very nature
is not self contained.

Well, this post aims to do exactly that.


## Intro

My day job involves designing and implementing distributed systems for
[Unison Cloud](unison.cloud), the next-gen cloud platform we're
building on top of the [Unison](unison-lang.org) language.

The first step in our journey is about a unique capability of our
Cloud: manipulating persistent and transactional storage as it was an
in-memory datastructure.

We expose this feature as a set of _abilities_ , Unison's take on
algebraic effects that can express custom control flow abstractions as
ordinary straight-line code.

The model is fairly simple: you write programs in the *Transaction*
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

As a quick syntax primer, `a ->{g} b` is a function from `a` to `b`
that performs effects defined by the `g` ability, `'{g} a` is
syntactic sugar for the thunk `() ->{g} a`, and lower case letters in
type signatures indicate generic type parameters.

Here's an example showing a bank transfer of 10 "pounds" (here
represented horribly as just a non-negative integer) between Alice and
Bob:

```haskell
accounts: Table UserId Nat
accounts = Table.Table "accounts"

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
```









