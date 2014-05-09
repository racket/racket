#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "plumbers"]{Plumbers}

A @deftech{plumber} supports @deftech{flush callbacks}, which are
normally triggered just before a Racket process or @tech{place} exits.
For example, a @tech{flush callback} might flush an output port's
buffer.@margin-note{@tech{Flush callbacks} are roughly analogous to the standard C
library's @as-index{@tt{atexit}}, but flush callback can also be used in other,
similar scenarios.}

There is no guarantee that a flush callback will be called before a
process terminates---either because the plumber is not the original
plumber that is flushed by the default @tech{exit handler}, or because
the process is terminated forcibly (e.g., through a custodian
shutdown).


@defproc[(plumber? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{plumber} value,
@racket[#f] otherwise.

@history[#:added "6.0.1.8"]}


@defproc[(make-plumber) pluber?]{

Creates a new @tech{plumber}.

Plumbers have no hierarchy (unlike @tech{custodians} or
@tech{inspectors}), but a @tech{flush callback} can be registered in
one plumber to call @racket[plumber-flush-all] with another plumber.

@history[#:added "6.0.1.8"]}


@defparam[current-plumber plumber plumber?]{

A @tech{parameter} that determines a @deftech{current plumber} for
@tech{flush callbacks}. For example, creating an output @tech{file
stream port} registers a @tech{flush callback} with the @tech{current
plumber} to flush the port as long as the port is opened.

@history[#:added "6.0.1.8"]}


@defproc[(plumber-flush-all [plumber plumber?]) void?]{

Calls all @tech{flush callbacks} that are registered with @racket[plumber].

The @tech{flush callbacks} to call are collected from @racket[plumber]
before the first one is called. If a @tech{flush callback} registers a
new @tech{flush callback}, the new one is @emph{not} called. If a
@tech{flush callback} raises an exception or otherwise escapes, then
the remaining @tech{flush callbacks} are not called.

@history[#:added "6.0.1.8"]}


@defproc[(plumber-flush-handle? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @deftech{flush handle}
represents the registration of a @tech{flush callback}, @racket[#f]
otherwise.

@history[#:added "6.0.1.8"]}


@defproc[(plumber-add-flush! [plumber plumber?]
                             [proc (plumber-flush-handle? . -> . any)]
                             [weak? any/c #f])
         plumber-flush-handle?]{

Registers @racket[proc] as a @tech{flush callback} with @racket[plumber], so
that @racket[proc] is called when @racket[plumber-flush-all] is
applied to @racket[plumber].

The result @tech{flush handle} represents the registration of the
callback, and it can be used with @racket[plumber-flush-handle-remove!] to
unregister the callback.

The given @racket[proc] is reachable from the @tech{flush handle}, but
if @racket[weak?] is true, then @racket[plumber] retains only a
@tech{weak reference} to the result @tech{flush handle} (and
thus @racket[proc]).

When @racket[proc] is called as a @tech{flush callback}, it is passed
the same value that is returned by @racket[plumber-add-flush!] so
that @racket[proc] can conveniently unregister itself. The call of
@racket[proc] is within a @tech{continuation barrier}.

@history[#:added "6.0.1.8"]}


@defproc[(plumber-flush-handle-remove! [handle plumber-flush-handle?]) void?]{

Unregisters the @tech{flush callback} that was registered by the
@racket[plumber-add-flush!] call that produced @racket[handle].

If the registration represented by @racket[handle] has been removed already,
then @racket[plumber-flush-handle-remove!] has no effect.

@history[#:added "6.0.1.8"]}
