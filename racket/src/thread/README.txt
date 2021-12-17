This thread implementation can be run in a host Racket with `make
demo`, but it's meant to be compiled for use in Racket on Chez Scheme;
see "../cs/README.txt", especially the "Modifying Racket" section.

Core engine support must be provided by a more primitive layer. The
more primitive layer must also provide `break-enabled-key` and special
handling for looking up a mark with that key so that an egine-specific
default thread cell is produced.

Futures
-------

The implementation of futures here has different behavior than in the
original Racket implementation:

 - Many more primitive operations are "safe" in the sense of allowing
   a future to keep running in parallel. For example, operations on
   mutable `eq?` and `eqv?` hash tables are safe, as are `equal?` and
   `equal-hash-code`.

   Any attempt to enter atomic mode at the level of Racket threads (as
   implemented in this layer) is treated as unsafe and will block the
   future. For example, attempting to use a port will invariably go
   into atomic mode, so port operations are still "unsafe". Any
   attempt to use a semaphore or channel will also involve atomic
   mode, so those are unsafe in the sense of blocking a fututre.

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

   To put is another way, there are no operations that trigger a 'sync
   logging output instead of a 'block output. That's main because so
   many more operations are safe. A second reason is that the futures
   implementation has a finer-grained view of a computation so that it
   doesn't see operations like `fprintf`; it sees only `start-atomic`
   as an unsafe step inside `fprintf`.
