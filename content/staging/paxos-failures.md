---
title: "A fault-driven understanding of Single-Decree Paxos"
---

Paxos is a mythical beast standing in the way of every budding
Distributed Systems Engineer. Classic Paxos is not _convoluted_, but
it uses deep ideas that resist immediate intuition.

We'll aid our understanding with a fault-driven approach, which is to
say: which specific failure mode is addressed by each idea in Paxos?
What makes each one necessary?

This post does not assume any prior knowledge of distributed systems,
and it strives to explain common jargon to enable you to go read the
relevant papers directly. Let's dive in.

## What is Paxos?

Paxos is a family of algorithms solving various flavours of the
_consensus problem_. The specific algorithm we're discussing today is
known interchangeably as Classic Paxos, Synod, or Single-Decree Paxos,
but this post will often just call it Paxos for brevity.

My favourite explanation of the exact kind of consensus Single-Decree
Paxos implements is by [Mahesh
Balakhrisnan](https://maheshba.bitbucket.io/blog/2021/11/15/Paxos.html)
, and we'll follow it here: Paxos implements a distributed data
structure known as a WOR, which stands for Write-Once Register.

The api of a WOR is minimal:

```
class WOR[A] {
  def write(v: A): Unit
  def read(): Null | A
}
```

but the types don't capture the full semantics, in particular:
- `write` can be called sequentially or concurrently by one or
  multiple processes, and it will only set the value once. All other
  successful calls to `write` are a no-op. This is what it means for
  the register to be _write-once_.
- `read` is allowed to return `null` when the register isn't set yet,
  but once it's set with value `v`, it will always return `v` and
  won't return `null` again. It will also never return any value from
  a call to `write` that didn't set the WOR. In other words, the WOR
  is _linearisable_, i.e it gives the illusion of operating as a
  single-thread, single-machine process, even though that's not the
  case internally.

The reason we're implementing a WOR using multiple machines is to
achieve _fault-tolerance_, and we will later describe exactly which
faults Paxos is designed to tolerate, and under which conditions. When
these conditions no longer hold, `read` and `write` will return an
error or time out, but they will never give inconsistent results:
linearisability is always respected.

A WOR might not seem like much, but we can use a sequence of WORs to
build a fault-tolerant, strongly consistent log, which can then be
used to persist the commands of an arbitrary state machine.

## System and Fault model

When describing a distributed algorithm, we have to specify a _fault
model_, i.e. which failure modes we are promising to deal with and
which ones we're ignoring, and a _system model_, i.e. which
capabilities we assume to be able to use to deal with them.

This part is often described in a couple of words of jargon at the
start of each paper and so it's easy to skim over for new
practitioners, but it's absolutely crucial, so we'll spend some time
explaining the model Paxos uses, which is an asychronous non-Byzantine
model with crash faults.

Basically, there is a set of processes that communicate by sending
messages to each other. These messages can be delayed, duplicated,
reordered, or dropped entirely. Even when delivered successfully, we
don't know exactly _when_ they are delivered (this is what
_asynchronous_ refers to), so we cannot say things like "if we don't
get a reply within `x` seconds, we know the message was dropped".
Processing is also asynchronous, so we don't know exactly when a
message will be processed.

We do assume that eventually messages will arrive and be processed, or
the algorithm cannot progress, but we won't be able to rely on this
information for correctness since we don't know when that will happen.
(In technical jargon: we are relying on partial synchrony for
liveness, but we provide safety under asynchrony).

Processes are allowed to restart and keep participating in the
algorithm, and are assumed to have access to stable storage. This
means that a process can come back up and remember state that it has
written when it was previously running.

However, processes are also allowed to crash, which means they stop
working and never resume working again. Their state is also
permanently lost. Calling this type of fault a _crash_ is common in
academic jargon, but crashing in general parlance indicates a
temporary failure, so we will use the word _explosion_ to avoid any
confusion.

Note that we won't be able to detect with certainty that a process has
exploded: in particular in the asynchronous model it's impossible to
distinguish an exploded process from a slow process, or from messages
being dropped.

Now, this model is looking pretty bleak, which is to say, fairly
realistic, but it's worth spelling out which faults it does not cover:

- The set of processes is fixed and known to all participants:
  _reconfiguration_ is not supported. This is not a realistic
  assumption in the real world, where we want to eventually replace
  machines that have exploded, or scale a cluster up and down.
  Reconfiguration is often left as an exercise for the reader, but
  it's actually very tricky and several papers are dedicated
  exclusively to it.
- There is no storage fault model. Storage goes away when a process
  explodes, but it's assumed to work reliably when the process is
  running. This is not fully realistic either as disk corruption in
  the medium term is very possible. Again, there are dedicated papers
  to this problem.
- The processes are assumed to be running the protocol correctly, and
  will not act maliciously. Similarly, messages won't be tampered
  with. In technical jargon, there are no _Byzantine faults_, which is
  actually an acceptable tradeoff for most systems, with the exception
  of blockchains.
    
## The Paxos Algorithm

Let's have a look at the actual algorithm, using the simplest, least
optimised version possible. What we're targeting here is a description
of what Single-Decree Paxos does, rather than an explanation of why it
does it. We will then see how each idea is necessary by showing which
failures it addresses.

We have client processes interacting with our WOR, which is in turn
implemented by a set of processes. Paxos divides these processes into
_proposers_ (writers), _acceptors_ (storage servers), and _learners_
(readers), although these roles don't have to be disjoint. We will
focus primarily on `write`, which is the core of the algorithm, so
we'll only talk about writers and storage servers.

How many instances of each of these processes should we have? Paxos
mandates lower bounds based on the number `f` of failures we want to
be able to tolerate. We need at least one writer to be up, so to
tolerate `f` failures, we need `f + 1` writers. We also need a
majority of storage servers to be up, so to tolerate `f` failures, we
need `2f + 1` storage servers. For example, a Paxos cluster with 5
writers and 5 storage servers can still function if 4 writers and 2
storage servers explode.

Writing is divided into Phase 1, where writers send `prepare` messages
and storage servers reply with `promise` messages, and Phase 2, where
writers send `propose` messages and storage servers reply with `accept`
messages. A real implementation will also have a bunch of negative
messages like `cannotAccept` as a basic optimisation, but here we'll
just avoid replying in that case, which lets us model all the
non-success cases as timeouts, since we need timeouts anyway to deal with
explosions and message loss.

A write can be attempted at any time, even when another attempt is in
progress, by the same writer or by another writer. It can also be
abandoned at any time, and the algorithm is robust to messages from
older attempts arriving late, when a new attempt is in progress. This
gives us a uniform strategy to deal with all kinds of errors or
uncertainty: we will just attempt the write again. Assuming there are
no more than `f` failures, eventually an attempt will succeed, either
setting the value, or with a no-op if another attempt has succeeded
unbeknownst to it.

 We'll call each attempt a _proposal_, and it will have the value `v`
we're trying to write, and a _proposal number_ `n`. Proposal numbers
should be unique and support a `<` comparison, but they don't have to
be consecutive. Paxos doesn't mandate a specific way of generating
proposal numbers, and there are a wealth of different designs with
various tradeoffs, so here's a simple one: each writer has a static
`process_id`, and a monotonically increasing integer `counter` which
is persisted to storage on each increment. The proposal number is then
`(counter, process_id)`.

When a client process contacts a writer to issue a write, the
algorithm proceeds as follows:

**Phase 1**:
1. The writer generates a new proposal number `n`.
2. The writer selects a _majority quorum_ of half the storage servers
   plus one, and sends `prepare(n)` to them.
3. When a storage server receives `prepare(n)`, if `n` is greater or
   equal than any `prepare` request it has previously responded to, it
   replies with `promise(n, maybe<n_, v_>)`, which is a promise to
   never `accept` any proposals numbered less than `n`. If it has
   `accept`ed a previous proposal, it also includes its proposal
   number `n_`, and the value `v_` it accepted. Storage servers
   remember the last promised proposal number and last accepted
   proposal, and write those values to stable storage before issuing
   any replies.
4. If the writer receives `promise(n, ...)` from all the storage
   servers it contacted, it proceeds to Phase 2. Otherwise if it times
   out, it will retry Phase 1 with a greater proposal number.

**Phase 2**:
1. The writer selects a value `v` to write to the WOR. If one of the
   `promise` replies from Phase 1 contains a previously `accept`ed
   proposal, it needs to select the value from that proposal. If
   there's more than one, it needs to select the value associated with
   the highest-numbered proposal. If no previously `accept`ed
   proposals are present in the `promise` replies, then it can select
   the value that the client is trying to write.
2. The writer sends `propose(n, v)` to all the storage servers
   selected in Phase 1.
3. When a storage server receives a `propose(n, v)`, it accepts the
   proposal unless it has responded to a `prepare` request with a
   number greater than `n`. Upon acceptance, it saves `n` and `v` to
   storage, and replies to the writer with `accept(n)`.
4. Once the writer receives `accept(n)` from all the storage servers
   it sent a `propose(n, v)` to, the write has either set a value or
   was a no-op, and the writer can return success to the client.
   Otherwise if it times out, it will retry Phase 1 with a greater
   proposal number.

Crystal clear, right 😛 ? When I first read this algorithm, I
found it absolutely mistifying. It's not that the rules are
particularly hard to follow, but... what are they for?

For example:
- Why are there 2 phases?
- Progress is predicated on receiving a response from a _majority_ of
  storage servers. Why a majority? Why not just one, or all of them,
  or 40% of them?
- There are rules about proposal numbers, what's the point of that?
- The writer in some cases proposes a value that's not the one the
  client sent. Why?
- In those cases, there's mention of _multiple_ such values. Why are
  there multiple values if the register is write-once?

And yet, believe it or not, this is the simplest thing that could
possibly work if we want fault-tolerance, and we will show that by
trying simpler algorithms and relentlessly breaking them.

## Failing our way to Paxos

The fundamental property that Paxos provides is _linearisability_. I
won't provide a formal definition here, but in the context of a WOR,
it means that it provides the following strong consistency guarantees:

1) A `write` only succeeds if there is a value in the WOR (not
  necessarily set by that `write`).
2) After a `write` succeeds, a `read` will never return `null`.
3) Multiple calls to `read`, even concurrent ones, will never see two
  different values.
4) The value seen by `read` is one that was sent by a `write` at some
   point. This rule is silly in practice but without it we could have
   a `WOR[Int]` that always returns 0, which respects rules 1-3.

### Synchronous Replication

The obvious implementation for the WOR rules is to have a single
storage server with stable storage, and the following logic:

- On `write(v)`, check if there's already a value saved to stable
  storage. If so, return success. If there is no value, write `v` to
  stable storage. After `v` is durable, return success.
- On `read`, read the value from stable storage.

Assuming that the implementation of single-machine concurrency and
persistence is bug-free, the above implements a correct WOR. Alas, it
is not fault-tolerant: if our storage server explodes then the WOR no
longer works.

Nothing can withstand an infinite amount of permanent failures, so
we'll parameterise our system with the maximum number `f` of faults it
can tolerate. We'll then have a number of storage servers that's
greater than `f`, so that even if `f` storage servers explode after
the value has been written, readers can still read it from the
remaining ones.

Each storage server will have the same logic as above, but we have to
make sure that a writer only returns success once the value has been
written to all the storage servers. This is the first big idea in
Paxos: _synchronous replication_.

This poses an issue though: if a write only completes once it's
acknowledged by _all_ the storage servers, then the explosion of even
just one storage server when the WOR is still empty will prevent any
writes from succeeding.

We'd like to modify the rule to require acks from _all the storage
servers that have not exploded_, but this is a dead end: there is no
way to reliably distinguish an exploded process from message loss or
delay.

Instead, we have to consider a write complete once it writes to a
_subset_ of storage servers, which begs the question, how big should
this subset be?

### Majority Quorums

The immediate issue with writing to a subset of storage servers is
that multiple writers can write different values to different subsets.

For example, in a cluster of 6 storage servers, we could have writer
`w1` write `v1` to 3 storage servers, and writer `w2` write `v2` to
another 3, which would then violate the WOR rules: two different calls
to `read` will get different values depending on which storage server
they hit.

We cannot guarantee that the same value is written to all the storage
servers, but what we can guarantee is that all readers draw the same
conclusion about what the overall value of the WOR should be.

This leads us straight to the second big idea in Paxos: _majority
quorums_.

Basically, a `write` should only succeed if it has a _quorum_, i.e. a
minimum number of storage servers that acknowledge that write, and
that quorum must be an _absolute majority_ , i.e. half of the storage
servers plus one. The simplest way to implement this requirement is to
select a majority quorum of servers to send a write message to, and
expect a successful response from all of them. Similarly, we want
`read` to be a quorum read, and only succeed if it reads the same
value from an absolute majority of storage servers.

This is correct because storage servers only persist the first value
that's sent to them, so it's impossible for _two_ values to be written
to an _absolute_ majority of storage servers, at least one server will
break any ties.

Since writes need a reply from a majority of storage servers to
succeed, this idea only works if less than a majority of storage
servers explode. This gives us a size requirement for our cluster: we
need `2f + 1` storage servers to tolerate `f` failures. We will later
see that this requirement is not only necessary, but also sufficient.

### 2-Phase Locking

Let's say we have 3 writers `{w1, w2, w3}` trying to write different
values to an empty WOR made of 3 storage servers `{s1, s2, s3}`.

All the writers start their writes concurrently, and imagine that the
first 3 events to happen are:

- `w1` writes `v1` to `s1`.
- `w2` writes `v2` to `s2`.
- `w3`writes `v3` to `s3`.

now there is no way for the algorithm to proceed correctly: no writer
has a quorum, and they will never get one as the storage servers don't
allow overwriting a value once written.

Overwrites are a no-go because they trivially break the write-once
property even without any concurrency: `w1` could successfully set an
empty WOR to `v1`, a reader would read `v1`, then 10 minutes later
`w2` arrives, overwrites all the values to successfully set `v2`, and
then the next reader reads `v2`. So, since we want to keep writes
idempotent on the storage servers, we'll have to change the logic of
the writers.

The core of the issue is that writers write to storage servers _too
eagerly_: a WOR write is only valid if its value is written to a
majority of storage servers, but by the time a writer realises it
doesn't have a majority, the damage is already done as some storage
servers have already been set.

A writer cannot actually find out whether or not it has a majority
before writing: even if it queries the storage servers, their state
might change right after the writer has received its response, but
before it can act on it.

Enter the third big idea in Paxos: _2-phase locking_.

The idea is that instead of trying to find out whether it has a
majority, a writer will try to _reserve_ one. Only once a majority
has been reserved will the writer send the value to storage servers,
hence why the process has 2 phases. The key component here is that
concurrent writers can retry the reserve phase multiple times until
one wins, without writing any value to storage servers until it is
safe to do so. The way we reserve a storage server is by preventing
further reservations until we're done, i.e. by _locking_ it.

So in Phase 1 the writer selects a majority of storage servers,
and sends them a `lock` message. When a storage server which isn't already
locked receives a `lock` message, it updates its lock status in
stable storage, and replies with `locked`.

If the writer receives `locked` replies from all the storage servers
it contacted, it proceeds to Phase 2. Otherwise if it times out, it
sends an `unlock` message to the storage servers it locked, and
retries Phase 1.

Eventually one writer will succeed locking a majority of storage
servers and advance to Phase 2, where it'll send `write(v)` to the
storage servers so that they can perform their idempotent write logic
as usual.


### Lock Stealing

2-phase locking provides fundamental concurrency control in the
presence of multiple writers, but our formulation in terms of
conventional locks assumes reliable execution of `lock` and `unlock`
commands so that locks are always in a consistent state whenever Phase
1 is retried.

In a distributed settings, this assumption is hilariously broken. For
example:
- `w1` and `w2` execute Phase 1 but fail to achieve quorum, which
  requires a retry. `w1` explodes before being able to send `unlock`
  messages, leaving some storage servers permanently locked, so `w2`
  cannot proceed.
- `w1` sends a `lock` message to `s1`, but doesn't receive a reply.
   Was the `lock` message dropped, and so `w1` didn't lock `s1`, or was
   the `locked` _reply_ dropped, and so `w1` did lock `s1`?
- An `unlock` message sent during a previous attempt gets duplicated,
  reordered, and arrives again much later, unlocking a storage server
  that was supposed to stay locked.

How do we get around the lack of consistency between `lock` and
`unlock`? The solution is surprising: we just eliminate `unlock`
messages. However, we now need a way to retry Phase 1 even when a
previous attempt has left some storage servers locked.

This is the fourth big idea in Paxos: _lock stealing_.

Instead of a boolean, the lock state on each storage server will be a
_version number_, and writers will try to lock at a certain version
number `n` by sending a `lock(n)` message. The key rule is that a
storage server locked at a certain version number can be locked again
by a _greater_ version number, or in other words, that lower-versioned
locks can be stolen. This allows writers to retry Phase 1 with a
greater version number if they cannot achieve a quorum.

Phase 1 becomes:
- A writer selects a version number `n`, and it sends `lock(n)` to a
  majority of storage servers.
- When a storage server receives `lock(n)`, it replies with
  `locked(n)` if `n` is greater or equal than the server's current
  lock version. Storage servers persist their lock version to stable
  storage before replying to any messages.
- If a writer receives `locked(n)` from all the storage servers it
  contacted, it proceeds with the write in Phase 2. If not, it will
  retry Phase 1 with a greater version number.

It's crucial to understand the nature of version numbers: they need to
be unique and support a `<` comparison, but _they don't have to be
consecutive_. Providing consecutive version numbers consistently with
distributed writers would be akin to consensus, and so we'd be back to
square one, but non-consecutive numbers are much easier.

Here's a basic scheme: each writer has a static `process_id`, and a
monotonically increasing integer `counter` which is persisted to
storage on each increment. The proposal number is then `(counter,
process_id)`, and storage servers compare `counter` first, and use
`process_id` as a discriminator. This specific scheme is not very fair
since a writer with low `process_id` will win any ties, and also not
super performant as each attempt requires a write to stable storage,
but there are more advanced schemes that fare better on both axes.





But we've recovered the property we're interested in: writers can
retry Phase 1 until they succeed in reserving a majority, and then
send their writes to storage servers in Phase 2.





2-phase locking works because writers can retry Phase 1 multiple times
until one writer succeeds in locking a majority of storage servers.
Well, can they? Retrying the locking phase assumes that writers
release any locks they have acquired during a failed attempt by
sending an `unlock` message to the respective storage servers.

TODO: a lot more failure modes, highlight some

But wait, there's no guarantee that those `unlock` messages will ever
be received! In particular, a writer can explode before being able to
send the `unlock` messages it needs to send.

This is a fundamental limitation of locks in a distributed setting,
and it motivates the next big idea in Paxos: _lock stealing_.


### Fencing

This is a good spot to talk about fencing, it wasn't required until lock stealing as writes were totally driven by the de-facto leader

talk about failure example with two servers

I can talk about the need for waiting for responses in Phase 2 here

First reconstruct the paxos messages from the locks we've discussed,

shape of paxos is starting to emerge, bullet points of paxos description with italic for the lock view explanation

then say what we're missing are value selection rules,
but before that, an interlude



### An alternate view: leaders and epochs (or maybe terms)

maybe name this section as "dueling leaders", and use that as a way to introduce the alternate view on locking

more intuitive, but less granular, useful to connect the dots, leaders and terms


### Write completion
this section can make the point about majorities having one acceptor
in common, after explaining the full mechanism: i.e. we cannot just
ignore and succeed if we see an old value (violates rule about success
if set), but also we cannot override it, (quorum loss on read). I then
have to explain the multiple values in different epochs, that's the
bit the requires the acceptor in common property.

btw we can explain the happy path case first, where we just contact a different majority of servers.

### Write Repair (we do allow rewriting)

this requires some inductive reasoning, show example with two
majorities, and explain how the only way a second value could have
appeared is if a writer didn't read any value from a majority, which
means one doesn't exist, as per the previous rule.

### Readers


## Conclusion

Single-Decree Paxos, especially the naively unoptimised version we
used today, is about the simplest consensus algorithm there is, and
yet it contains a surprising amount of extremely deep ideas.

In fact, I look at Paxos in general not as an algorithm, but rather a
set of foundational ideas that can be assembled into a variety of
solutions to consensus, from Jacks of all trades like Raft to [very
specialised ones](https://neon.tech/blog/paxos).

We looked at some of these ideas today, teasing them out from their
initially cryptic formulation with the help of the Distributed Systems
Engineer's constant companion: failure.

If you enjoyed this post, we're reimagining what it means to program
distributed programs with [Unison Cloud](https://www.unison.cloud/).
Go check it out, and see you next time!

