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
The approach I'm going to use today is a _failure-first
understanding_, which is to say: which specific failure mode is
addressed by each idea in Paxos? Which bad thing will happen if you
don't follow them?

This article will not assume any prior knowledge about distributed systems, and failure-first thinking is a useful mindset to get into since the failure modes caused by distribution are much more treachearous than the single machine case. The invariant-first approach
