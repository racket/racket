This directory implements the port, path, encoding, printing, and
formatting layer. It can be run in a host Racket with `zuo . demo`,
which is useful for development and debugging, but it's meant to be
compiled for use in Racket on Chez Scheme; see "../cs/README.txt",
especially the "Modifying Racket" section.

Core error support must be provided as a more primitive layer,
including the exception structures and error functions that do not
involve formatting, such as `raise-argument-error`. The more primitive
layer should provide a `error-value->string-handler` paramemeter, but
this layer sets that parameter (so the primitive error functions like
`raise-argument-error` won't work right until this layer is loaded).

Thread and event support is similarly provided as a more primitive
layer, but running `zuo . demo` doesn't rely on it.

Locks
-----

See "../thread/README.txt" for an introduction to atomic mode,
uninterruptible mode, and the custodian locks from the perspective of
the thread layer.

This layer is implemented mostly with rktio, which has specific
concurrency opportunities and requirements as documented in "rktio.h".
For the most part, access to rktio functionality uses a rktio-specific
mutex-based lock. See "host/rktio.rkt" for more information.

Each input and output port has its own lock. The lock uses a CAS
(compare-and-set) operation in the common case, but when a port uses
synchronizable events from the thread layer, taking the lock will
enter atomic mode, not just interruptible mode. Working with multiple
ports at a time, as in `subprocess`, also can force a port lock to
atomic mode. See "port/lock.rkt" for more information. The rktio lock
is ordered after port locks.

The custodian lock from the thread layer is ordered after port and
rktio locks.

The thread layer offers a debugging mode that checks lock ordering,
and that debugging mode has this layer's noition of locks built into
it (even though it's at the wrong layer). See "../cs/thread.sls".
