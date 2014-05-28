#lang scribble/doc
@(require "mz.rkt")

@title{Exiting}

@defproc[(exit [v any/c #t]) any]{

Passes @racket[v] to the current @tech{exit handler}. If the exit
handler does not escape or terminate the thread, @|void-const| is
returned.}


@defparam[exit-handler proc (any/c . -> . any)]{

A @tech{parameter} that determines the current @deftech{exit handler}. The
@tech{exit handler} is called by @racket[exit].

The default @tech{exit handler} in the Racket executable
takes any argument, calls @racket[plumber-flush-all] on the original plumber,
and shuts down the OS-level Racket process. The
argument is used as the OS-level exit code if it is an exact integer
between @racket[1] and @racket[255] (which normally means
``failure''); otherwise, the exit code is @racket[0], (which normally
means ``success'').}


@defparam[executable-yield-handler proc (byte? . -> . any)]{

A @tech{parameter} that determines a procedure to be called as the Racket
process is about to exit normally. The procedure associated with this
parameter is not called when @racket[exit] (or, more precisely, the
default @tech{exit handler}) is used to exit early. The argument to
the handler is the status code that is returned to the system on exit.
The default executable-yield handler simply returns @|void-const|.

The @racketmodname[scheme/gui/base] library sets this parameter to
wait until all frames are closed, timers stopped, and queued events
handled in the main eventspace. See @racketmodname[scheme/gui/base]
for more information.}
