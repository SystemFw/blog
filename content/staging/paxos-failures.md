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
Paxos implements is by Mahesh Balakhrisnan (TODO: link), and we'll
follow it here: Paxos implements a distributed data structure known as
a WOR, which stands for Write-Once Register.

The api of a WOR is minimal:

```
class Wor[A] {
  def write(a: A): Unit
  def read(): Null | A
}
```

but the types don't capture the full semantics, in particular:
- `write` can be called sequentially or concurrently by one or
  multiple processes, and it will only succeed once. All other calls
  to `write` are a no-op. This is what it means for the register to be
  _write-once.
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


