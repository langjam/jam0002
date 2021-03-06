# 1 Create Diagrams
I used draw.io (aka diagrams.net).

I created 5 pattern diagrams:

1. Nesting (aka Structured Programming)
2. Race Condition
3. Daisy Chain
4. Deadlock Prevention
5. Request/Acknowledge

## Discussion
### Nesting
"Nesting" is also know as "structured programming" and "scoping".
### Race Condition
Physics says that there is only 1 race condition - i.e. 2 events that arrive so close together that we can't tell which one came first.[^1]

What this means is *not* that the two events actually arrived "simultaneously'", only that our hardware isn't fast enough to tell which came first.  Different hardware gives different results for what is "fast enough".  

Each kind of hardware has a "time window" which determines how fast the hardware can respond and which determines the width of the time window.  If 2 events arrive within the same (small) time window, the hardware can't tell which one came first.  

In the 1950's, CPUs had time windows of micro-seconds.  2020's CPUs have time windows of nano-seconds.

All other race conditions are forms of accidental complexity, usually caused by sharing memory.  

Memory sharing was in vogue in the 1950's due efficiency concerns ("premature optimization" in retrospect).  These 1950s solution to this kind of accidental complexity gave us threads, but, threading is rife with hidden kinds of accidental complexity (e.g. priority inversion, fairness, etc., etc.).

##### Solution Overview
The solution to this problem is to use a 4-state state machine.  

Let's say that there are 2 events called A and B.

In state 0, neither event has arrived.

If A arrives first, we wait for a B.  Call this "State 1".

If B arrives first, we wait for an A. Call this "State 2".

In State 1, when we see a B, we move to State 3.

In State 2, when we see an A, we move to State 3.

In State 3, both events have arrived, and, we can proceed. 

(In this JAM, I ignore error conditions, like and A arriving while we are waiting for a B).

Often, we don't actually care about the arrival order.  We only care that we have read, both, an A and a B, and, that our hardware did not miss any events[^1].

[^1]: Actually, it is possible to miss some events during the moment that we transition from State 0 to State 1 or 2, but the above solution minimizes this possibility.  Low-level hardware deals with this situation by setting the *overrun* flag.

When we arrive in State 3, we can proceed to process the events (data, etc.).

### Daisy Chain

A daisy chain passes data down a chain of receivers and handles all "locking" issues[^2].

[^2]: Traditional multitasking solutions use semaphores for this kind of thing and make the programmer worry about this stuff.  In this component-driven version, locking only matters when enqueing and dequeing messages.  The underlying code is simple.  The user (the programmer) does not need to worry about locking - the underlying code does all the work.

Each node in the chain examines the data.  If a node can process the data, it does so.  If it cannot process the data, it passes it on to the next node in the chain, like a baton in a relay race.

Daisy chains can be used 
- to offload work (aka `load balancer`)", or,
- to route information to specific nodes.

The daisy chain is not `fair`.  The first node in the chain gets first dibs on packets that come through.  `Fairness` is over-rated, though.  We really don't care about `fairness`, we only care that the work gets done.  `Fairness` comes with its own accidental complexities, because, `fairness` is often implemented using `shared memory` and `time-sharing`.  These are bad ideas, and should be avoided if possible.  This problem was unavoidable in the 1950s, but can be avoided in the 2020s (e.g. internet, IoT).  

Why pay for something you don't need?  If you use a classical operating system (e.g. Linux, Windows, MacOS), you are wasting CPU cycles running code that implements `time-sharing` and `memory sharing` and `fairness`.  These operating systems allow many apps to run on the same computer without stepping on each other.  If you run only 1 app which takes over the whole computer (e.g. a game, IoT, an internet server), then you don't need to deal with the problems of `fairness`, etc.

BTW, daisy chaining is a technique that is used in TTL hardware design.

### Deadlock Prevention

The most-obvious way to prevent deadlock[^3] is to create a server which manages the shared resources.

[^3]: Deadlock happens when multiple clients try to grab/use the same resouce (e.g. a file system, or, some piece of hardware, etc.).  When a client wants to use 2 resources, it grabs the 1st one and "locks" it. Then, it tries to grab the 2nd resouce.  If, in the meantime, another client has grabbed and locked the 2nd resource and, now, tries to grab the 1st resource, we get classical "deadlock".  A client can't proceed because it doesn't hold locks on all of the resources it needs, but the resouces it needs are locked by another client.  The client will wait forever.

If you squint hard, you will see that every so-called solution to the deadlock prevention problem is a variant of the above solution (often done at a micro-management level).  In many cases, the "server" code is distributed across several threads (which solves yet a different problem - load balancing).

Micro-managed solutions to this problem bring their own baggage of accidental complexities.

### Request / Acknowledge

If a node processes really large data, then it needs to receive the data in chunks.

This used to be a real problem in the 1950s, when memory and CPUs were expensive.

In the 2020s, this is still a problem, but the datasets have become larger

The only way to receive data in chunks is to *ask* for it.  This is termed a `REQuest`.

The upstream data generator/reader/whatever sends the data in an "ACKnowledge" packet in a manageable chunk size.

[If an error occurs, the upstream node sends a NAK (Not ACknowledge).]

#### Bounded Buffers As A Solutin To The Chunked Data Problem
Bounded buffers are considered to be a solution to this problem, but bounded buffers are actually implemented as REQ/ACK protocols using threads.  

If an error occurs, the sender sends a NAK (Not AcKnowledge), or, the receiver times out.

If you use a bounded buffer[^3], you are paying the cost of using threads and an REQ/ACK protocol.

[^3]: Bounded buffers are usually implemented under-the-hood in operating systems at the system level.  Operating systems are merely libraries of code.

#### Futures As A Solution To The Chunked Data Problem

A `future` is another way that REQ/ACK is presented[^2] to the programmer.

[^2]: Note that I choose to use the word "presented" instead of "implemented".  There is only 1 way to implement a bounded buffer at the opcode level.  You can only change this by re-designing the hardware (not likely).

At the low level, a `future` must be able to automatically suspend the sender and the receiver.  This usually means the use of threads or the use of state machines.

Gendanken Q: What is the cost of using a `future` in whatever language you use?  Are you trying to squeeze blood out of your CPU?  If so, you need to know what's going on underneath.  While you're at it, ask what is the cost of using a thread?  What is the cost of using an operating system?

# Diagrams
I drew the diagrams in draw.io (aka diagrams.net) and have saved them as .svg files in this directory.

## Nesting
<img src="https://github.com/guitarvydas/jam0002/blob/main/guitarvydas/patterns-1.%20Nesting.svg"/>
## Race Condition
<img src="https://github.com/guitarvydas/jam0002/blob/main/guitarvydas/patterns-2.%20Race%20Condition.svg"/>
## Daisy Chain
<img src="https://github.com/guitarvydas/jam0002/blob/main/guitarvydas/patterns-3.%20Daisy%20Chain.svg"/>
## Deadlock Prevention
<img src="https://github.com/guitarvydas/jam0002/blob/main/guitarvydas/patterns-4.%20Deadlock%20Prevention.svg"/>
## Request - Acknowledge
<img src="https://github.com/guitarvydas/jam0002/blob/main/guitarvydas/patterns-5.%20Request%20-%20Acknowledge.svg"/>

# Appendix - Call / Return Spaghetti
This article discusses the concepts of how to create minimal multi-threaded objects.

https://guitarvydas.github.io/2020/12/09/CALL-RETURN-Spaghetti.html