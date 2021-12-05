# Plan for GameJam 0002
0. Theme is "patterns".
1. Create diagrams for some patterns.
2. Write code in C for some of the diagrammed patterns.
3. Use `d2f` and `f2j` to convert one (at least) diagram to JSON.
4. Compile diagram from JSON to C.
5. Repeat, using C++.
6. Document.


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

Each kind of hardware has a "time window" which determines how fast the hardware can respond and which determines the width of the time window.  If 2 events arrive within the same (small) time window, the hardware can't tell which one came first.  1950's CPUs had time windows of micro-seconds.  2020's CPUs have time windows of nano-seconds.

All other race conditions are forms of accidental complexity, usually caused by sharing memory.  Memory sharing was in vogue in the 1950's due to premature efficiency concerns.  These 1950s solution to this kind of accidental complexity gave us threads, but, these were rife with hidden kinds of accidental complexity (e.g. priority inversion)) and so on.

The solution to this problem is to use a 4-state state machine.  

Let's say that there are 2 events called A and B.

In state 0, neither event has arrived.

If A arrives first, we wait for a B.  State 1.

If B arrives first, we wait for an A. State 2.

In State 1, when we see a B, we move to state 3.

In State 2, when we see an A, we move to state 3.

In State 3, both events have arrived, and, we can tell in which order they were seen by our hardware.

Often, we don't actually care about the arrival order.  We only care that we have read, both, an A and a B, and, that our hardware did not miss any events[^1].

[^1]: Actually, it is possible to miss some events during the moment that we transition from State 0 to State 1 or 2, but the above solution minimizes this possibility.  Low-level hardware deals with this situation by setting the *overrun* flag.

From State 3, we can proceed to process the events (data, etc.).

### Daisy Chain

A daisy chain passes data down a chain of receivers and handles "locking" issues.

Each node in the chain examines the data.  If a node can process the data, it does so.  If it cannot process the data, it passes it on to the next node in the chain, like a baton in a relay race.

Daisy chains can be used 
- to offload work (aka `load balancer`)", or,
- to route information to specific nodes.

The daisy chain is not `fair`.  The first node in the chain gets first dibs on packets that come through.  `Fairness` is over-rated, though.  We really don't care about `fairness`, we only care that the work gets done.  `Fairness` comes with its own accidental complexities, because, `fairness` is often implemented using `shared memory` and `time-sharing`.  These are bad ideas, and should be avoided if possible.  This problem was unavoidable in the 1950s, but can be avoided in the 2020s (e.g. internet, IoT).  

Why pay for something you don't need?  If you use a classical operating system (e.g. Linux, Windows, MacOS), you are wasting CPU cycles running code that implements `time-sharing` and `memory sharing` and `fairness`.  These operating systems allow many apps to run on the same computer without stepping on each other.  If you run only 1 app which takes over the whole computer (e.g. a game), then you don't need to deal with the problems of `fairness`, etc.

BTW, daisy chaining is a technique that is used in TTL hardware.

### Deadlock Prevention

The most-obvious way to prevent deadlock is to create a server which manages the shared resources.

If you squint hard, you will see that every so-called solution to the deadlock prevention problem is a variant of the above solution (often done at a micro-management level).  In many cases, the "server" code is distributed across several threads (which solves yet a different problem - load balancing).

Micro-managed solutions to this problem bring their own baggage of accidental complexities.

### Request / Acknowledge

If a node processes really large data, then it needs to receive the data in chunks.

This used to be a real problem in the 1950s, when memory and CPUs were expensive.

In the 2020s, this is still a problem, but the datasets have become larger

The only way to receive data in chunks is to *ask* for it.  This is termed a `REQuest`.

The upstream data generator/reader/whatever sends the data in an "ACKnowledge" packet in a manageable chunk size.

#### Bounded Buffers
Bounded buffers are considered to be a solution to this problem, but bounded buffers are actually implemented as REQ/ACK protocols using threads.  

If an error occurs, the sender sends a NAK (Not AcKnowledge), or, the receiver times out.

If you use a bounded buffer[^2], you are paying the cost of using threads and an REQ/ACK protocol.

[^2]: Bounded buffers are usually implemented under-the-hood in operating systems at the system level.  Operating systems are merely libraries of code.

#### Futures

A `future` is another way that REQ/ACK is presented[^2] to the programmer.

[^2]: Note that I choose to use the word "presented" instead of "implemented".  There is only 1 way to implement a bounded buffer at the opcode level.  You can only change this by re-designing the hardware (not likely).

At the low level, a `future` must be able to automatically suspend the sender and the receiver.  This usually means the use of threads or the use of state machines.

Gendanken Q: What is the cost of using a `future` in whatever language you use?  Are you trying to squeeze blood out of your CPU?  If so, you need to know what's going on underneath.  While you're at it, ask what is the cost of using a thread?  What is the cost of using an operating system?

# Diagrams
I drew the diagrams in draw.io (aka diagrams.net) and have saved them as .svg file in this directory.

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
