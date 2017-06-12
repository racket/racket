The "rktio" library (for "Racket I/O") is a layer just above the OS
layer to provide a portable interface to filesystem, networking, etc.
facilities.

The library is meant to be

 * easily embeddable;

 * always non-blocking;

 * independent of global state (except on Windows, where internal
   global state is managed appropriately with locks, and except for
   Unix process handling without pthreads), so that it works with or
   without threads; and

 * easily callable though a FFI.

Many such libraries exist already. This one happens to have exactly
the things that a Racket implementation needs.
