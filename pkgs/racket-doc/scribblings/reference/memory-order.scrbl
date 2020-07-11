#lang scribble/doc
@(require "mz.rkt"
          (for-label racket/unsafe/ops))

@title[#:tag "memory-order"]{Machine Memory Order}

Unlike Racket @tech{threads}, futures and places can expose the
underlying machine's memory model, including a weak memory ordering.
For example, when a future writes to multiple slots in a mutable
vector, it's possible on some platforms for another future to observe
the writes in a different order or not at all, unless the futures are
explicitly synchronized. Similarly, shared byte strings or
@tech{fxvectors} can expose the machine's memory model across places.

Racket ensures that a machine's memory model is not observed in a way
that unsafely exposes the implementation of primitive datatypes. For
example, it is not possible for one future to see a partially
constructed primitive value as a result of reading a vector that is
mutated by another future.

The @racket[box-cas!], @racket[vector-cas!],
@racket[unsafe-box*-cas!], @racket[unsafe-vector*-cas!], and
@racket[unsafe-struct*-cas!] operations all provide a machine-level
compare-and-set, so they can be used in ways that are specifically
supported by the a machine's memory model. The
@racket[(memory-order-acquire)] and @racket[(memory-order-release)]
operations similarly constrain machine-level stores and loads.
Synchronization operations such as place messages, future
@racket[touch]es, and @tech{future semaphores} imply suitable
machine-level acquire and release ordering.

@deftogether[(
@defproc[(memory-order-acquire) void?]
@defproc[(memory-order-release) void?]
)]{

Those operations implement a machine-level memory fence on platforms
where one is needed for synchronization. The
@racket[memory-order-acquire] operation ensures at least a load--load
and load--store fence at the machine level, and the
@racket[memory-order-release] operation ensures at least a
store--store and store--load fence at the machine level.

@history[#:added "7.7.0.11"]}
