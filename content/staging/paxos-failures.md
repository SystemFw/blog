---
title: "A failure-first understanding of Single Decree Paxos"
date: 2025-05-06
---

Paxos is a mythical beast standing in the way of every budding
Distributed Systems Engineer. Classic Paxos is actually not _that_
convoluted as distributed algorithms go, but it uses several deep
ideas that are challenging to truly understand at first. Deep ideas
tend to resist a single, all-encompassing explanation, and are best
approached from multiple angles, until intuition comes. 
The angle I'm going to use today is _failure-first understanding_,
which is to say: which specific failure mode is addressed by each idea
in Paxos? Which bad thing can happen that makes each idea necessary?

I picked this approach because new practictioners underestimate the
variety and subtlety of the failure modes introduced by distribution.
The invariant-first approach that Leslie Lamport (Paxos' creator) uses
is also very insightful, but can come later.

This post does not assume any prior knowledge of distributed systems.

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
class Wor[A] {
  def write(a: A): Unit
  def read(): Null | A
}
```

but the types don't capture the full semantics, in particular:
- `write` can be called sequentially or concurrently by one or
  multiple processes, and it will only succeed in setting the value
  once. All other calls to `write` are a no-op. This is what it means
  for the register to be _write-once_.
- `read` is allowed to return `null` when the register isn't set yet,
  but once it's set, it will always return `A` and won't return `null`
  again. It will also never return any `A` from a call to `write` that
  didn't succeed in setting the WOR. In other words, the WOR is
  _linearisable_, i.e it gives the illusion of operating as a
  single-thread, single-machine process, even though that's not the
  case internally.

You might be wondering why we need to involve multiple machines when
implementing a WOR, and the answer is that we want _fault-tolerance_.
We will soon see which faults Paxos is designed to prevent, and under
which conditions.

This post will primarily focus on writes, which is where the bulk of
the complexity lies.


## System and Fault model

When describing a distributed algorithm, we have to specify a _fault
model_, i.e. which failures we are promising to deal with and which
ones we're ignoring, and a _system model_, i.e. which capabilities we
assume to be able to use to deal with them.

This part is often described in a couple of words of jargon at the
start of each paper and so it's easy to skim over for new
practitioners, but it's absolutely crucial, so we'll spend some time
explaining the model Paxos uses, which is an asychronous non-Byzantine
model with crash faults.

Basically, there is a set of processes that communicate by sending
messages to each other.
These messages can be delayed, reordered, or dropped entirely. Even
when delivered successfully, we don't know exactly _when_ they are
delivered (this is what _asynchronous_ refers to), so we cannot say
things like "if we don't get a reply within x seconds, we know the
message was dropped". Processing is also asynchronous, so we don't
know exactly when a message will be processed.

We do assume that eventually messages will arrive and be processed, or
the algorithm cannot progress, but we won't be able to rely on this
information for correctness since we don't know when that will happen.
(In technical jargon: we are relying on partial synchrony
for liveness, but we only assume asynchrony for safety).

Processes are allowed to restart and keep participating in the
algorithm, and are assumed to have access to stable storage. This
means that a process can come back up and remember state that it has
written when it was previously running.

However, processes are also allowed to _crash_, which means they stop
working, and never resume working again. Their state is also
permanently lost.

Note that we won't be able to detect with certainty that a process has
crashed: in particular in this model it's impossible to distinguish a
crashed process from a slow process, or from messages being dropped.

Of course no guarantees can be given if all processes crash, so we
will later see how Paxos defines a maximum number of crash failures it
can tolerate before it no longer works. For Paxos, _not working_ means
that operations don't succeed, but they never return inconsistent
results.


anything else before we describe things that aren't covered (reconfig, storage faults, byzantine failures)
