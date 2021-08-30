#lang scribble/doc
@(require "utils.rkt")

@cs-title[#:tag "cs-thread"]{Managing OS-Level Threads}

Chez Scheme functionality can only be accessed from OS-level threads
that are known to the Chez Scheme runtime system. Otherwise, there's a
race condition between such an access and a garbage collection that is
triggered by other threads.

A thread not created by Chez Scheme can be made known to the runtime
system by activating it with @cppi{Sactivate_thread}. As long as a
thread is active by not running Chez Scheme code, the thread prevents
garbage collection in all other running threads. Deactivate a thread
using @cppi{Sdeactivate_thread}.

@function[(int Sactivate_thread)]{

Activates the current OS-level thread. An already-activated thread can
be activated again, but each activation must be balanced by a
decativation. The result is @cpp{0} if the thread was previously
activated @cpp{1} otherwise.}

@function[(void Sdeactivate_thread)]{

Deactivates the current OS-level thread---or, at least, balances on
activation, making the thread deactive if there are no remaining
activations to balance with deactivation.}

@function[(int Sdestroy_thread)]{

Releases any Chez Scheme resources associated with the current OS
thread, which must have been previously activated but which must not
be activated still.}
