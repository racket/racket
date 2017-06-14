The "rktio" library (for "Racket I/O") is a layer just above the OS
layer to provide a portable interface to filesystem, networking, etc.
facilities.

The library is meant to be

 * easily embeddable;

 * always non-blocking;

 * independent of global state, so that it works with or without
   threads; and

 * easily callable though a FFI.

Many such libraries exist already. This one happens to have exactly
the things that a Racket implementation needs.

============================================================

Allocation conventions:

 * Unless otherwise specified, returned data must be deallocated ---
   using a type-specific deallocation function if provided or
   rktio_free() otherwise. The rktio_free() function is the same as
   free().

 * There's no reference counting. If object A refers to object B, then
   a client must keep object B alive as long as object A exists.

 * String arguments are copied if they must be retained. Unless
   otherwise specified, creating an object A with string S doesn't
   require that S stay live as long as A exists. String results are
   generally allocated and must be freed by the client.

Return type conventions:

 * A return type `rktio_ok_t` (alias for `int`) means that 1 is
   returned for succes and 0 for error. Use
   rktio_get_last_error_kind() and rktio_get_last_error() for more
   information about a 0 result.

 * A return type `rktio_tri_t` (alias for `int`) means that 0 is
   returned for an expected failuree, some `RKTIO_...` (alias for 1)
   is returned for success, and `RKTIO_...ERROR` (alias for -2) is
   returned for some error. Use rktio_get_last_error_kind() and
   rktio_get_last_error() for more information about a
   `RKTIO_...ERROR` result.

 * For a pointer return type, unless otherwise specified, a NULL
   result means an error. Use rktio_get_last_error_kind() and
   rktio_get_last_error() for more information about the error.

 * If a function returns `void`, you can rely on it to not change the
   error reported by rktio_get_last_error_kind() and
   rktio_get_last_error().

Thread conventions:

 * A given `rktio_t` can be used from only one thread at a time.
   Otherwise, as long as the initial call to rktio_init() returns
   before a second call, there are no threading requirements.

Signals:

 * SIGCHLD may be enabled, blocked, and/or handled.
