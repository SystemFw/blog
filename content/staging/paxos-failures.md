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
unbeknownst to us.

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
2. The writer selects a _ majority quorum_ of half the storage servers
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

### Replication

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

### Quorums

The issue with writing to a subset of storage servers is that multiple
writers can write different values to different subsets.

For example, in a cluster of 6 storage servers, we could have Writer A
write `v1` to 3 storage servers, and Writer B write `v2` to another 3,
which would then violate the WOR rules: two different calls to `read`
will get different values depending on which storage server they hit.


 Solving this problem requires the
second big idea in Paxos: _majority quorums_.

The idea is that since we cannot guarantee that the same value is
written to all the storage servers, we have to guarantee that each
reader draws the _same_ conclusion about what the overall value of the
WOR should be.

We can achieve that by requiring writes to be _quorum_ writes a write only succeeds if it can write the value of half the storage servers plus one, i.e. an absolute majority of servers: Since there cannot be two absolute majorities, we have a guarantee that only a single value is 



Let's instead say that the winner requires an _absolute_ majority,
i.e. it has to be written to half the storage servers plus one.
This means that `write` has to be a _quorum_ write: it can only succeed
- This guarantees only one value is chosen as the value of the WOR
  since there cannot be two absolute majorities.


### Replication

The obvious implementation for the WOR rules is to have a single
storage server with stable storage:

- `write(v)` checks if there's already a value saved to stable
  storage. If so, it returns success. If there is no value, it writes
  `v` to stable storage. After `v` is durable, it returns success.
- `read` reads the value from stable storage.

In practice we also have to make sure that single-machine
multithreading is done correctly, but remember that one of the
assumptions in our model is that each algorithm is implemented
properly.

This is simple and correct but not fault-tolerant, if our storage
server explodes then the WOR no longer works.

Let's apply the same logic to multiple storage servers, with one
caveat: if all of them explode, then clearly nothing would work, so we
need to parameterise our algorithm with the number of faults `f` it
can tolerate.

How many storage servers do we need for a given `f`? Well, at least
`f + 1`, so that even if `f` of them explode after the value has been
written, there is still one that remembers the value. A writer just
has to make sure to only return success once the value has been
written to all `f + 1` storage servers. This is the first big idea in
Paxos: _replication_.

So to tolerate 4 failures, we'd have 5 storage servers, and a writer
would write to all 5 before returning success. But now we have another
issue: if even just one storage server explodes _before_ any value has
been written, then no write would ever complete, as it would never be
able to write to 5 storage servers!

You might be thinking that if one storage server has exploded, then
writing to 4 of them and returning success is fine since our model
only allows another 3 to fail, but remember that in the asynchronous
model we cannot reliably distinguish explosions from message loss or
delay.

TODO: these sections ^ v have the same issue as the quorums section, I
need to clearly explain quorums for consistency before I go too deep
into fault tolerance, or we need to talk about majority quorums for
durability, and then that requires talking about write repair early.
Talking about `f + 1` rather than `g > f` also leads to wanting to
talk about durability quorums and then eventually write repair.

So here's what can happen:
1) A writer writes `v` to 4 storage servers, but doesn't get a reply from
  the 5th. After waiting for a while and/or retrying, it declares it
  exploded and returns success.
2) The 5th storage server is still running, but all messages to and
  from it were dropped.
3) The 4 servers that contain the value all explode, which is allowed
  by our model since `f = 4`.
4) A reader reaches the 5th server, and reads `null`.
5) This breaks WOR rule no.2: we cannot read `null` after a write has
   succeeded.
  
So, to summarise:
- if the rule is to write to all storage servers, then one early
  explosion prevents any future writes. Instead, we're going to have
  to consider a write valid even if it writes to a subset of storage
  servers.
- If we write to number of storage servers that is `< f`, then reads
  aren't fault-tolerant. Therefore, `f + 1` storage servers are not
  enough.


### Quorums

We need to increase the number of storage servers, but now this poses a problem of consistency: since we 


TODO: this section should not make precise claims about fault
tolerance or you need to get into write repair. Instead, we'd just say
we increase storage servers, and then talk about consistency. We can
then frame the 2f + 1 acceptors as a requirements for writes to be
possible.

Let's add more storage servers. Say we're tolerating 2 failures, so `f
= 2`, and we have 8 storage servers. How many of those does a writer
need to write to before returning success?

We could try 4 out of 8, so that:
- if there are `f = 2` explosions before a write, the writer still has
  enough storage servers to write to.
- if `f = 2` out of the 4 storage servers that contain the value
  explode after a write, the value is still stored by the other 2.
  
We have fault tolerance! Except...we now break linearisability even
without any failures.

The issue is that two writers that are competing to write different
values to the WOR can reach disjoint sets of 4 storage servers, and so
they both succeed. Then different reads will return different values
depending on which set they hit. Solving this problem requires the
second big idea in Paxos: _majority quorums_.

The idea is that since we cannot guarantee that the same value is
written to all the storage servers, we have to guarantee that each
reader draws the _same_ conclusion about what the overall value of the
WOR should be.

We can achieve that by requiring writes to be _quorum_ writes a write only succeeds if it can write the value of half the storage servers plus one, i.e. an absolute majority of servers: Since there cannot be two absolute majorities, we have a guarantee that only a single value is 

One way we could do that is to say that the value that is written to a
relative majority of storage servers wins: e.g. if we have `A` on 3
storage servers, `B` on 2 and `C` on 2, then `A` wins. However, this
requires having a complete picture of the state of _all_ the storage
servers, which is again not resilient to explosions.

Let's instead say that the winner requires an _absolute_ majority,
i.e. it has to be written to half the storage servers plus one.
This means that `write` has to be a _quorum_ write: it can only succeed
- This guarantees only one value is chosen as the value of the WOR
  since there cannot be two absolute majorities.
- It's also fault tolerant: since we can ignore any values written on
  a minority of servers, we can allow a minority to fail and still
  have the real value.
  
^ This view is confusing since it then gets into needing write repair
to complete reads.



For this to work, we need quorum reads and quorum writes, i.e. a write is only successful if it 


This requires `read` to only contact half the storage servers plus one: it can ignore the others because they 

The idea is that we don't need to have the same value written to all
the storage servers , which would require unanymous writes and 
, _as long as each reader can draw the same
conclusion about what the value of the WOR should be_.


I think i have to spell out the idea that we will allow multiple values on different acceptors

This is best understood by thinking of `read`: multiple writers
have written different values to different storage servers, how does
`read` decide which value is the actual value of the WOR?

We could say that whatever value is written to a relative majority of storage server "wins", but this would require knowing the state of _all_ storage servers
absolute majority

if multiple writers are competing to write different values to different sets of storage servers, how does th


Let's call _quorum_ the group of storage servers that a writer has to
write a value `v` to before returning success. When a whole quorum replies to the writer of a value `v`, we can say that the qu

The issue is that two quorum that are disjoint can take different decisions on what the value of the WOR should be

A majority quorum is defined as half the storage servers plus one,
which guarantees the key property that any two such quorums would have
at least one storage server in common: it's impossible for two quorums
to be disjoint (see it `servers =`)

A majority quorum is defined as half the storage servers plus one,
which guarantees the key property that any two such quorums would have
at least one storage server in common: it's impossible for two quorums
to be disjoint (see it `servers =`)

For example, if we have storage servers A, B, C, possible quorums are {A, B}, {A, C}, {B, C}, 

But this hints at a solution too: we want the writer to succeeds 

### 2-Phase Locking

### Lock stealing

### Write repair
this section can make the point about majorities having one acceptor
in common, after explaining the full mechanism: i.e. we cannot just
ignore and succeed if we see an old value (violates rule about success
if set), but also we cannot override it, (quorum loss on read). I then
have to explain the multiple values in different epochs, that's the
bit the requires the acceptor in common property.

---


So clearly we need to write the value of the WOR to storage in multiple processes.


more than `f`. All machines, we cannot write if one fails (no oracle).
how many? if a random number, then we don't have linear..ty. we need
quorums so that one machine is a decider (reads have quorums). but
then we get stuck with minorities, and rejected writes. If writes can
be overriden, they will both override it (with a read in between). we
need locking to avoid early visibility

write to all machines --> unavailable for writes
quorums --> one machine decides ultimately, requires quorum reads
one failure --> stuck forever. Actually use 6 storage 3 writers for
this rather than failure, so that I can explain completing writes
later.
overwrite proposals --> lin broken if it gets read the critical
machine crashes. Maybe skip this as it gets into completing writes as
well, just talk about writes being too eager.
overwrite proposals: two writers both override, with a read in between
2pl --> unlock failures
lock stealing --> completing writes
completing writes --> multiple failed writes
that's full paxos

## Conclusion

Single-Decree Paxos, especially the naively unoptimised version we
used today, is about the simplest consensus algorithm there is, and
yet it contains a surprising amount of extremely deep ideas.

In fact, I look at Paxos in general not as an algorithm, but rather a
set of foundational ideas that can be assembled into a variety of
solutions, from Jacks of all trades like Raft to [very specialised
ones](https://neon.tech/blog/paxos).

We looked at some of these ideas today, teasing them out from their
initially cryptic formulation with the help of the Distributed Systems
Engineer's constant companion: failure.

If you enjoyed this post, we're reimagining what it means to program
distributed programs with [Unison Cloud](https://www.unison.cloud/).
Go check it out, and see you next time!
