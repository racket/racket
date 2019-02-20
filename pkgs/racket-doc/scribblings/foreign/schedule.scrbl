#lang scribble/doc
@(require "utils.rkt"
          (for-label ffi/unsafe/schedule))

@title{Thread Scheduling}

@defmodule[ffi/unsafe/schedule]{The
@racketmodname[ffi/unsafe/schedule] library provides functions for
cooperating with the thread scheduler and manipulating it. The
library's operations are unsafe because callbacks run in @tech{atomic
mode} and in an unspecified thread.}

@history[#:added "6.11.0.1"]

@defproc[(unsafe-poller [poll (evt? (or/c #f any/c) . -> . (values (or/c #f list?) evt?))])
         any/c]{

Produces a @deftech{poller} value that is allowed as a
@racket[prop:evt] value, even though it is not a procedure or itself
an @racket[evt?]. The @racket[poll] callback is called in @tech{atomic
mode} in an unspecified thread to check whether the event is ready or
to allow it to register a wakeup trigger.

The first argument to @racket[poll] is always the object that is used
as a @tech[#:doc reference.scrbl]{synchronizable event} with the
@tech{poller} as its @racket[prop:evt] value. Let's call that value
@racket[_evt].

The second argument to @racket[poll] is @racket[#f] when @racket[poll]
is called to check whether the event is ready. The result must be two
values. The first result value is a list of results if @racket[_evt]
is ready, or it is @racket[#f] if @racket[_evt] is not ready. The
second result value is @racket[#f] if @racket[_evt] is ready, or it is
an event to replace @racket[_evt] (often just @racket[_evt] itself) if
@racket[_evt] is not ready.

When the thread scheduler has determined that the Racket process
should sleep until an external event or timeout, then @racket[poll] is
called with a non-@racket[#f] second argument, @racket[_wakeups]. In
that case, if the first result value is a list, then the sleep will be
canceled, but the list is not recorded as the result (and @racket[poll]
most likely will be called again). In addition to returning a @racket[#f] initial
value, @racket[poll] can call @racket[unsafe-poll-ctx-fd-wakeup],
@racket[unsafe-poll-ctx-eventmask-wakeup], and/or
@racket[unsafe-poll-ctx-milliseconds-wakeup] on @racket[_wakeups] to
register wakeup triggers.}


@defproc[(unsafe-poll-fd [fd exact-integer?]
                         [mode '(read write)]
                         [socket? any/c #t])
         boolean?]{

Checks whether the given file descriptor or socket is currently ready
for reading or writing, as selected by @racket[mode].

@history[#:added "7.2.0.6"]}


@defproc[(unsafe-poll-ctx-fd-wakeup [wakeups any/c]
                                    [fd fixnum?]
                                    [mode '(read write error)])
         void?]{

Registers a file descriptor (Unix and Mac OS) or socket (all
platforms) to cause the Racket process to wake up and resume polling
if the file descriptor or socket becomes ready for reading, writing,
or error reporting, as selected by @racket[mode]. The @racket[wakeups]
argument must be a non-@racket[#f] value that is passed by the
scheduler to a @racket[unsafe-poller]-wrapped procedure.}


@defproc[(unsafe-poll-ctx-eventmask-wakeup [wakeups any/c]
                                           [mask fixnum?])
         void?]{

On Windows, registers an eventmask to cause the Racket process to wake
up and resume polling if an event selected by the mask becomes
available.}


@defproc[(unsafe-poll-ctx-milliseconds-wakeup [wakeups any/c]
                                              [msecs flonum?])
         void?]{

Causes the Racket process will wake up and resume polling at the point
when @racket[(current-inexact-milliseconds)] starts returning a value
that is @racket[msecs] or greater.}

@defproc[(unsafe-set-sleep-in-thread! [foreground-sleep (-> any/c)]
                                      [fd fixnum?])
         void?]{

Registers @racket[foreground-sleep] as a procedure to implement
sleeping for the Racket process when the thread scheduler determines
at the process will sleep. Meanwhile, during a call to
@racket[foreground-sleep], the scheduler's default sleeping function
will run in a separate OS-level thread. When that default sleeping
function wakes up, a byte is written to @racket[fd] as a way of
notifying @racket[foreground-sleep] that it should return
immediately.

This function works on when OS-level threads are available within the
Racket implementation. It always works for Mac OS.}

@defproc[(unsafe-signal-received) void?]{

For use with @racket[unsafe-set-sleep-in-thread!] by
@racket[_foreground-sleep] or something that it triggers, causes the
default sleeping function to request @racket[_foreground-sleep] to
return.}
