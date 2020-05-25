#lang scribble/doc
@(require "mz.rkt"
          (for-label racket/engine))

@title[#:tag "engine"]{Engines}

@note-lib-only[racket/engine]

An @deftech{engine} is an abstraction that models processes that
can be preempted by a timer or other external trigger. They are
inspired by the work of Haynes and Friedman @cite["Haynes84"].

Engines log their behavior via a logger with the name 
@racket['racket/engine]. The logger is created when the module
is instantiated and uses the result of @racket[(current-logger)]
as its parent. The library adds logs a @racket['debug] level
message: when @racket[engine-run]
is called, when the engine timeout expires, and when the engine is
stopped (either because it terminated or it reached a safe point to
stop). Each log message holds a value of the struct:
@racketblock[(struct engine-info (msec name) #:prefab)]
where the @racket[_msec] field holds the result of 
@racket[(current-inexact-milliseconds)] at the moment of logging,
and the @racket[_name] field holds the name of the procedure
passed to @racket[engine].

@defproc[(engine [proc ((any/c . -> . void?) . -> . any/c)])
         engine?]{

Returns an engine object to encapsulate a thread that runs only when
allowed. The @racket[proc] procedure should accept one argument, and
@racket[proc] is run in the engine thread when
@racket[engine-run] is called. If @racket[engine-run] returns
due to a timeout, then the engine thread is suspended until a
future call to @racket[engine-run]. Thus, @racket[proc] only
executes during the dynamic extent of a @racket[engine-run] call.

The argument to @racket[proc] is a procedure that takes a boolean, and
it can be used to disable suspends (in case @racket[proc] has critical
regions where it should not be suspended). A true value passed to the
procedure enables suspends, and @racket[#f] disables
suspends. Initially, suspends are allowed.}


@defproc[(engine? [v any/c]) any]{

Returns @racket[#t] if @racket[v] is an engine produced by
@racket[engine], @racket[#f] otherwise.}


@defproc[(engine-run [until (or/c evt? real?)][engine engine?])
         boolean?]{

Allows the thread associated with @racket[engine] to execute for up
as long as @racket[until] milliseconds (if @racket[until] is a real
number) or @racket[until] is ready (if @racket[until] is an event). If
@racket[engine]'s procedure disables suspends, then the engine
can run arbitrarily long until it re-enables suspends.

The @racket[engine-run] procedure returns @racket[#t] if
@racket[engine]'s procedure completes (or if it completed earlier),
and the result is available via @racket[engine-result].  The
@racket[engine-run] procedure returns @racket[#f] if
@racket[engine]'s procedure does not complete before it is
suspended after @racket[timeout-secs]. If @racket[engine]'s
procedure raises an exception, then it is re-raised by
@racket[engine-run].}


@defproc[(engine-result [engine engine?]) any]{

Returns the result for @racket[engine] if it has completed with a
value (as opposed to an exception), @racket[#f] otherwise.}


@defproc[(engine-kill [engine engine?]) void?]{

Forcibly terminates the thread associated with @racket[engine] if
it is still running, leaving the engine result unchanged.}
