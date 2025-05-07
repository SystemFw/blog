---
title: "What is scalability?"
date: 2025-05-06
---

"Will it scale?" is just about the most common question you hear in an
engineering discussion, but a clear understanding of what scalability
actually is is more subtle than it might seem. In particular,
scalability is often conflated with performance, which doesn't account
for the fact that a more scalable system can be (and often is TODO:
link COST) less performant than a non-scalable one. Typical interview
answers, on the other hand, seem to focus almost exclusively on _how_
scalability can be achieved, e.g. vertically vs horizontally, again
without a clear understanding of _what_ we're trying to achieve in the
first place.

The first step to a better understanding is a definition of
_performance_ : performance measures how much of a given resource or
set of resources is consumed to perform a given task. 
For example we might measure how much _time_ is consumed to perform a
given task (_latency_), or how many _items of work_ are consumed in a
given amount of time (_throughput_), or how many items of work are
consumed _successfully_ (_goodput_).

The resources
involved often, but not always, include _time_: for example we might
measure how much time it takes to perform a given instance of the task
at hand (_latency_), or fix the unit of time and see how many
instances of the task can be performed in that unit (_throughput_), or
measure the consumption of resources that  involve time at all,
such as memory or CPU utilisation.

So, what is scalability then? I'll give a mathy definition, followed
by an example involving cake: scalability is _the first derivative of
performance with respect to load_, i.e. it measures how performance
varies as we change the load.

Let's imagine Batman and Robin are having a cake-eating context, 

I feel like really we need two words: 
A) derivative of perf wrt to load
B) derivative of A wrt to resources

maybe the post should be: 2 words for scalability

I need to add the dimension of adding resources, and angle it more as
"How do I think of scalability"
