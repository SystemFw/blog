---
title: "A failure-first understanding of Single Decree Paxos"
date: 2025-05-06
---

TODO: standardise on fault vs failure. fault-driven understanding?

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

This post does not assume any prior knowledge of distributed systems,
and it strives to explain common jargon to enable you to go read the
relevant papers directly.

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

The reason we're implementing a WOR using multiple machines is to
achieve _fault-tolerance_, and we will later describe exactly which
faults Paxos is designed to tolerate, and under which conditions. When
these conditions no longer hold, `read` and `write` will return an
error or time out, but they will never give inconsistent results:
linearisability is always respected.

This post will focus on `write`, which highlights all the core ideas.
Reading does have a couple of subtleties as well though, it might be
the topic of a future post.

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
(In technical jargon: we are relying on partial synchrony for
liveness, but we provide safety under asynchrony).

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

Now, this model is looking pretty bleak, which is to say, fairly
realistic, but it's worth spelling out which faults it does not cover:

- The set of processes is fixed, and known to all participants. It can
  shrink as a result of crash failures, but it won't grow, nor will
  ever be replaced by another set of processes. In other words,
  _reconfiguration_ is not supported. This is not a realistic
  assumption in the real world, where we want to eventually replace
  machines that have crashed, or scale a cluster up and down.
  Reconfiguration is often left as an exercise for the reader, but
  it's actually very tricky and several papers are dedicated
  exclusively to it.
- There is no storage fault model. Storage goes away when a process
  crashes, but it's assumed to work reliably when the process is
  running. This is not realistic either as disk corruption in the
  medium term is very possible. Again, there are dedicated papers to
  this problem.
- The processes are assumed to be run the protocol correctly, and will
  not act maliciously. Similarly, messages won't be tampered with. In
  technical jargon, there are no _Byzantine faults_, which is actually
  an acceptable tradeoff for most systems, with the notable exception
  of blockchains.
    
## The Paxos Algorithm

Let's have a look at the actual algorithm, using the simplest, least
optimised version possible. What we're targeting here is a description
of what Single Decree Paxos does, rather than an explanation of why it
does it. We will then see how each idea is necessary by showing which
failures it addresses.

We have client processes interacting with our WOR, which is in turn
implemented by a set of processes. Paxos divides these processes into
_proposers_, _acceptors_ , and _learners_, although these roles don't
have to be disjoint. We are focusing on `write` here, so we only need
proposers, i.e. _writers_, and acceptors, i.e. _storage servers_. How
many instances of each of these processes should we have? Paxos
mandates lower bounds based on the number `f` of failures we want to
be able to tolerate. We need at least one writer to be up, so to
tolerate `f` failures, we need `f + 1` writers. We also need a
majority of storage servers to be up, so to tolerate `f` failures, we
need `2f + 1` storage servers. For example, a Paxos cluster with 5
writers and 5 storage server can still function if 4 writers and 2
storage servers crash.


