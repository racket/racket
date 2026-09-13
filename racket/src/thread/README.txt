This thread implementation can be run in a host Racket with `make
demo`, but it's meant to be compiled for use in Racket on Chez Scheme;
see "../cs/README.txt", especially the "Modifying Racket" section.

Core "engine" support must be provided by a more primitive layer ---
that is, the ability to run a continuation for a while and interrupt
it based on a timer. The more primitive layer must also provide
`break-enabled-key` and special handling for looking up a mark with
that key so that an engine-specific default thread cell is produced.

Engine
------

A coroutine thread or parallel thread normally runs inside an engine,
which is responsible for running a continuation for some number of
steps and then pausing it, maybe to check for breaks. The coroutine
thread scheduler (in "schedule.rkt") drives engines, and so some of
its implementation lives outside an engine. Similarly, a scheduler for
futures or the scheduler of a thread pool for parallel threads (more
precisely: the worker Chez Scheme threads that belong to that
scheduler) drives engines.

When a thread or future is doing scheduler-sensitive work, it needs to
go into "uninterruptible" mode with `start-uninterruptible` and
`end-uninterruptible`, at least.

The `start-atomic` and `end-atomic` functions imply
`start-uninterruptible` and `end-uninterruptible`, but `start-atomic`
further insists on being in a coroutine thread, because it's doing
something that the coroutine thread is sensitive to. When
`start-atomic` is used not in a coroutine thread, it will block or
move over to a coroutine thread; see "Futures" and "Parallel Threads"
below. Within a coroutine thread, if the scheduler wanted to interrupt
the thread but it was in atomic mode, then it queues a function to be
called when `end-atomic` is reached; that's among the reasons that
`start-atomic` should not be balanced by `end-uninterruptible`.

The `current-atomic` thread register (at the Chez Scheme thread level)
technically should be called `current-uninterruptible-level`, but
`current-atomic` is more succinct. Similarly, `in-atomic-mode?` and
`not-atomic-mode?` more precisely check for uninterruptible mode.

Thread
------

"Thread" or "Racket thread" by default means a Racket coroutine
thread. These are coroutines or "green threads" within a place; they
do not run in parallel with each other. Synchronization at this level
is often implemented by using atomic mode via `start-atomic` and
`end-atomic`.

Anything that touches the coroutine thread scheduler and the
`thread-...` family of functions needs to be running in a coroutine
thread. As explained further below, if a relevant operation is tried
in a parallel thread, it will block and send the operation to a
coroutine thread.

There are places in the implementation of threads where atomic mode
has been exited, but the thread isn't supposed to be running anymore,
and it's on its way to being descheduled. In those places, it's
important to use `end-atomic/no-barrier-exit`, so that the
continuation isn't moved to the future for a parallel thread (where
the running coroutine thread is part of the implementation of a
parallel thread). See "Parallel Threads" below.

Futures
-------

A future is implemented by a continuation that runs in a Chez Scheme
thread different from the one used by coroutine threads. Anything that
shouldn't run in a future (because it inspects or modifies certain
state) should be guarded by `start-atomic`, `future-block` (called
indirectly by `start-atomic`), or `future-barrier` (which calls
`future-block` if an inlined, fast-path check determines that
`future-block` may be necessary).

The implementation of futures here has different behavior than in the
original Racket BC implementation:

 - Many more primitive operations are "safe" in the sense of allowing
   a future to keep running in parallel. For example, operations on
   mutable `eq?` and `eqv?` hash tables are safe, as are `equal?` and
   `equal-hash-code`.

   Any attempt to enter atomic mode at the level of Racket threads (as
   implemented in this layer) is treated as unsafe and will block the
   future. For example, a read or write through a port that needs to
   wait for data will go into atomic mode, so some port operations are
   still "unsafe". Any attempt to use a semaphore or channel will also
   involve atomic mode, so those are unsafe in the sense of blocking a
   future.

   Attempting to use any operation that depends on the current
   continuation or the current thread will also block, as it must.
   However, a continuation-sensitive operation can succeed if it is
   delimited by a prompt that is shallower in the continuation than
   the future's start; that's an improvement over the original Racket
   implementation.

 - A blocked future becomes permanently ineligible for running in
   parallel. That is, if a future hits an unsafe operation, the future
   won't switch to the main thread just long enough to perform the
   operation and then switch back to parallel mode.

   To put this another way, there are no operations that trigger a
   'sync logging output for a future instead of a 'block output.
   That's main because so many more operations are safe. A second
   reason is that the futures implementation has a finer-grained view
   of a computation so that it doesn't see operations like `fprintf`;
   it sees only `start-atomic` as an unsafe step inside `fprintf`.

Each future has a lock that owns the future's representation. Taking a
future's lock implies `start-uninterruptible`, and releasing the lock
implies `end-uninterruptible` (and crucially to many contexts, not
`end-atomic`).

Parallel Threads
----------------

A Racket parallel thread as created by `(thread #:pool p)` turns into
a call of `thread/parallel` at this layer. A parallel thread is
implemented by a combination of a coroutine thread and a future. The
future is special in two ways: (1) when the future blocks (e.g., hits
`start-atomic`), the accompanying coroutine thread takes over
automatically, as if by `touch`; and (2) a continuation that was moved
to a coroutine thread can move back to the future after the blocking
operation is handled (e.g., at `end-atomic`). This difference is
reflected in logging, where operations that would 'block for a future
are 'sync operations for a parallel thread.

The internal coroutine thread associated with a parallel thread is
used as the external representative of the parallel thread. If it is
shut down or sent a break signal, the future is terminated or
signaled accordingly.

When `end-atomic` exits atomic mode, then it calls
`future-barrier-exit`, which is an inlined check. If it detects the
potential for an associated parallel future, it calls the more general
`future-unblock` function. The `future-unblock` function detects
precisely when a parallel future is available to continue the work of
the current coroutine thread, and it kicks the continuation back over
to the parallel thread's future if so.

Locks
-----

In addition to uninterruptible and atomic mode, the this layer has a
few locks that are implemented as host-supplied mutexes. These
generally must be take in uninterruptible mode, so that a thread is
not swapped out or otherwise suspended while it holds a lock.

The custodian lock is the only one that isn't purely internal. A
custodian can only be shut down in a thread that is in atomic mode and
also holds the custodian lock. Meanwhile, an object can be registered
with a custodian or unregistered using only the custodian lock. The
lock is exposed, so that a `custodian-closed?` check, registration,
and unregistration can be uninterruptably combined with other
operations.

The interaction of a garbage-collection callback and custodians is
governed by a lock that is different than the custodian lock. That
lock can be taken only with GC interrupts disabled.
