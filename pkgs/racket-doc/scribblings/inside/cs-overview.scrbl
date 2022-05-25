#lang scribble/doc
@(require "utils.rkt")

@cs-title[#:tag "cs-overview"]{Overview}

The Racket CS runtime system is implemented by a wrapper around the
Chez Scheme kernel. The wrapper implements additional glue to the
operating system (e.g., for I/O and networking) and provides entry
points into the Racket layer's evaluator.

@; ----------------------------------------------------------------------

@section{``S'' versus ``Racket''}

In the C API for Racket CS, names that start with @cpp{S} are from the
Chez Scheme layer, while names that start with @cpp{racket_} are from
the Racket wrapper.

@; ----------------------------------------------------------------------

@section[#:tag "cs-memory"]{Racket CS Memory Management}

@index['("allocation")]{Racket} values may be moved or garbage
collected any time that @cpp{racket_...} functions are used to run
Racket code. Do not retain a reference to any Racket value across such
a call. This requirement contrasts with the BC implementation of
Racket, which provides a way for C code to more directly cooperate with
the memory manager.

API functions that start with @cpp{S} do not collect or move objects
unless noted otherwise, so references to Racket values across such
calls is safe.

The @cpp{Slock_object} function can prevent an object from being moved
or garbage collected, but it should be used sparingly. Garbage
collection can be disabled entirely by calling the Chez Scheme
function @tt{disable-interrupts}, and then reenabled with a balancing
call to @tt{enable-interrupts}; access those functions via
@cpp{racket_primitive} and call them via @cpp{Scall0}. Beware that
disabling interrupts also disables context switching for Racket
threads and signal handling for breaks. Assuming that interrupts start
out enabled, calling @tt{disable-interrupts} could trigger a garbage
collection before further collections are disabled.

@; ----------------------------------------------------------------------

@section[#:tag "cs-places"]{Racket CS and Places}

Each Racket @|tech-place| corresponds to a Chez Scheme thread, which
also corresponds to an OS-implemented thread. Chez Scheme threads
share a global allocation space, so GC-managed objects can be safely
be communicated from one place to another. Beware, however, that Chez
Scheme threads are unsafe; any synchronization needed to safely share
a value across places is must be implemented explicitly. Racket-level
functions for places will only share values across places when they
can be safely used in both places.

In an @seclink["cs-embedding"]{embedding application}, the OS thread
that originally calls @cpp{racket_boot} is the OS thread of the
original place.

@; ----------------------------------------------------------------------

@section{Racket CS and Threads}

Racket implements threads for Racket programs without aid from the
operating system or Chez Scheme's threads, so that Racket threads are
cooperative from the perspective of C code. Stand-alone Racket uses a
few private OS-implemented threads for background tasks, but these
OS-implemented threads are never exposed by the Racket API.

Racket can co-exist with additional OS-implemented threads, but care
must be taken when calling @cpp{S} functions, and additional OS or
Chez Scheme threads must not call any @cpp{racket_} function. For
other OS threads to call @cpp{S} functions, the thread must be first
activated as a Chez Scheme thread using @cppi{Sactivate_thread}.


@; ----------------------------------------------------------------------

@section[#:tag "cs-intsize"]{Racket CS Integers}

The C type @cpp{iptr} is defined by Racket CS headers to be an integer
type that is big enough to hold a pointer value. In other words, it is
an alias for @cpp{intptr_t}. The @cpp{uptr} type is the unsigned variant.
