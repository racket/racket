#lang scribble/doc
@(require "mz.ss")

@title{Exiting}

@defproc[(exit [v any/c #t]) any]{

Passes @scheme[v] to the current @tech{exit handler}. If the exit
handler does not escape or terminate the thread, @|void-const| is
returned.}


@defparam[exit-handler proc (any/c . -> . any)]{

A parameter that determines the current @deftech{exit handler}. The
@tech{exit handler} is called by @scheme[exit].

The default @tech{exit handler} in the @exec{mzscheme} executable
takes any argument and shuts down the OS-level Scheme process. The
argument is used as the OS-level exit code if it is an exact integer
between @scheme[1] and @scheme[255] (which normally means
``failure''); otherwise, the exit code is @scheme[0], (which normally
means ``success'').}
