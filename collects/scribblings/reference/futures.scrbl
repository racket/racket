#lang scribble/doc
@(require "mz.rkt" (for-label racket/future))

@(define future-eval (make-base-eval))
@(interaction-eval #:eval future-eval (require racket/future))

@(define time-id @racketidfont{time})

@title[#:tag "futures"]{Futures}

@guideintro["effective-futures"]{futures}

@note-lib[racket/future]

@margin-note{Currently, parallel support for @racket[future] is enabled
  by default for Windows, Linux x86/x86_64, and Mac OS X x86/x86_64. To
  enable support for other platforms, use @DFlag{enable-futures} with
  @exec{configure} when building Racket.}

The @racket[future] and @racket[touch] functions from
@racketmodname[racket/future] provide access to parallelism as supported
by the hardware and operating system.  In contrast to @racket[thread],
which provides concurrency for arbitrary computations without
parallelism, @racket[future] provides parallelism for limited
computations. A @deftech{future} executes its work in parallel (assuming that
support for parallelism is available) until it detects an attempt to
perform an operation that is too complex for the system to run safely in
parallel. Similarly, work in a future is suspended if it depends in some
way on the current continuation, such as raising an exception. A
suspended computation for a future is resumed when @racket[touch] is
applied to the future.

``Safe'' parallel execution of a future means that all operations
provided by the system must be able to enforce contracts and produce
results as documented. ``Safe'' does not preclude concurrent access to
mutable data that is visible in the program.  For example, a computation
in a future might use @racket[set!] to modify a shared variable, in
which case concurrent assignment to the variable can be visible in other
futures and threads. Furthermore, guarantees about the visibility of
effects and ordering are determined by the operating system and
hardware---which rarely support, for example, the guarantee of
sequential consistency that is provided for @racket[thread]-based
concurrency. At the same time, operations that seem obviously safe may
have a complex enough implementation internally that they cannot run in
parallel. See also @guidesecref["effective-futures"] in @|Guide|.

A future never runs in parallel if all of the @tech{custodians} that
allow its creating thread to run are shut down. Such futures can
execute through a call to @racket[touch], however.

@deftogether[(
  @defproc[(future [thunk (-> any)]) future?]
  @defproc[(touch [f future?]) any]
)]{

  The @racket[future] procedure returns a future value that encapsulates
  @racket[thunk].  The @racket[touch] function forces the evaluation of
  the @racket[thunk] inside the given future, returning the values
  produced by @racket[thunk].  After @racket[touch] forces the evaluation
  of a @racket[thunk], the resulting values are retained by the future
  in place of @racket[thunk], and additional @racket[touch]es of the
  future return those values.

  Between a call to @racket[future] and @racket[touch] for a given
  future, the given @racket[thunk] may run speculatively in parallel to
  other computations, as described above.

  @interaction[
    #:eval future-eval
    (let ([f (future (lambda () (+ 1 2)))])
      (list (+ 3 4) (touch f)))
]}

@defproc[(futures-enabled?) boolean?]{
  Returns whether parallel support for futures is enabled 
  in the current Racket configuration.
}

@defproc[(current-future) (or/c #f future?)]{

  Returns the descriptor of the future whose thunk execution is the
  current continuation.  If a future thunk itself uses @racket[touch],
  future-thunk executions can be nested, in which case the descriptor of
  the most immediately executing future is returned.  If the current
  continuation is not a future-thunk execution, the result is
  @racket[#f].
}


@defproc[(future? [v any/c]) boolean?]{

  Returns @racket[#t] if @racket[v] is a future value, @racket[#f]
  otherwise.

}

@defproc[(would-be-future [thunk (-> any)]) future?]{
  Returns a future that never runs in parallel, but that consistently
  logs all potentially ``unsafe'' operations during the execution of
  the future's thunk (i.e., operations that interfere with parallel
  execution).

  With a normal future, certain circumstances might prevent the logging
  of unsafe operations. For example, when executed with debug-level logging,
  
  @racketblock[
    (touch (future (lambda () 
                     (printf "hello1") 
                     (printf "hello2") 
                     (printf "hello3"))))] 

  might log three messages, one for each @racket[printf] 
  invocation.  However, if the @racket[touch] is performed before the future
  has a chance to start running in parallel, the future thunk evaluates
  in the same manner as any ordinary thunk, and no unsafe operations
  are logged.  Replacing @racket[future] with @racket[would-be-future] 
  ensures the logging of all three calls to @racket[printf]. 
}

@defproc[(processor-count) exact-positive-integer?]{

  Returns the number of parallel computation units (e.g., processors or
  cores) that are available on the current machine.

}

@defproc[(make-fsemaphore [init exact-nonnegative-integer?]) fsemaphore?]{

  Creates and returns a new @deftech{future semaphore} with the
  counter initially set to @racket[init].

  A future semaphore is similar to a plain @tech{semaphore}, but
  future-semaphore operations can be performed safely in parallel (to synchronize
  parallel computations). In contrast, operations on plain @tech{semaphores}
  are not safe to perform in parallel, and they therefore prevent
  a computation from continuing in parallel.

}

@defproc[(fsemaphore? [v any/c]) boolean?]{

  Returns @racket[#t] if @racket[v] is an @tech{future semaphore}
  value, @racket[#f] otherwise.

}

@defproc[(fsemaphore-post [fsema fsemaphore?]) void?]{

  Increments the @tech{future semaphore}'s internal counter and
  returns @|void-const|.

}

@defproc[(fsemaphore-wait [fsema fsemaphore?]) void?]{

  Blocks until the internal counter for @racket[fsema] is non-zero.
  When the counter is non-zero, it is decremented and
  @racket[fsemaphore-wait] returns @|void-const|.

}

@defproc[(fsemaphore-try-wait? [fsema fsemaphore?]) boolean?]{

  Like @racket[fsemaphore-wait], but @racket[fsemaphore-try-wait?]
  never blocks execution.  If @racket[fsema]'s internal counter is zero,
  @racket[fsemaphore-try-wait?] returns @racket[#f] immediately without
  decrementing the counter.  If @racket[fsema]'s counter is positive, it
  is decremented and @racket[#t] is returned.

}

@defproc[(fsemaphore-count [fsema fsemaphore?]) exact-nonnegative-integer?]{

  Returns @racket[fsema]'s current internal counter value.

}

@; ------------------------------------------------------------

@section[#:tag "future-logging"]{Future Performance Logging}

Racket futures use logging (see @secref["logging"]) extensively to
report information about how futures are evaluated. Logging output is
useful for debugging the performance of programs that use futures.

In addition to its string message, each event logged for a future has
a data value that is an instance of a @racket[future-event]
@tech{prefab} structure:

@racketblock[
(define-struct future-event (future-id
                             proc-id
                             action
                             time
                             unsafe-op-name)
  #:prefab)
]

The @racket[future-id] field is an exact integer that identifies a
future, or it is @racket[#f] when @racket[action] is
@racket['missing]. The @racket[future-id] field is particularly useful
for correlating logged events.

The @racket[proc-id] fields is an exact, non-negative integer that
identifies a parallel process. Process 0 is the main Racket process,
where all expressions other than future thunks evaluate.

The @|time-id| field is an inexact number that represents time in
the same way as @racket[current-inexact-milliseconds].

The @racket[action] field is a symbol:

@itemlist[

 @item{@racket['create]: a future was created.}

 @item{@racket['complete]: a future's thunk evaluated successfully, so
       that @racket[touch] will produce a value for the future
       immediately.}

 @item{@racket['start-work] and @racket['end-work]: a particular
       process started and ended working on a particular future.}

 @item{@racket['start-0-work]: like @racket['start-work], but for a
       future thunk that for some structural reason could not be
       started in a process other than 0 (e.g., the thunk requires too
       much local storage to start).}

 @item{@racket['start-overflow-work]: like @racket['start-work], where
       the future thunk's work was previously stopped due to an
       internal stack overflow.}

 @item{@racket['sync]: blocking (processes other than 0) or initiation
       of handing (process 0) for an ``unsafe'' operation in a future
       thunk's evaluation; the operation must run in process 0.}

 @item{@racket['block]: like @racket['sync], but for a part of
       evaluation that must be delayed until the future is
       @racket[touch]ed, because the evaluation may depend on the
       current continuation.}

 @item{@racket['touch] (never in process 0): like @racket['sync] or
       @racket['block], but for a @racket[touch] operation within a
       future thunk.}

 @item{@racket['overflow] (never in process 0): like @racket['sync] or
       @racket['block], but for the case that a process encountered an
       internal stack overflow while evaluating a future thunk.}

 @item{@racket['result] or @racket['abort]: waiting or handling for
       @racket['sync], @racket['block], or @racket['touch] ended with
       a value or an error, respectively.}

 @item{@racket['suspend] (never in process 0): a process blocked by
       @racket['sync], @racket['block], or @racket['touch] abandoned
       evaluation of a future; some other process may pick up the
       future later.}

 @item{@racket['touch-pause] and @racket['touch-resume] (in process 0,
       only): waiting in @racket[touch] for a future whose thunk is
       being evaluated in another process.}

 @item{@racket['missing]: one or more events for the process were lost
       due to internal buffer limits before they could be reported,
       and the @|time-id| field reports an upper limit on the time
       of the missing events; this kind of event is rare.}

]

Assuming no @racket['missing] events, then @racket['start-work],
@racket['start-0-work], @racket['start-overflow-work] is always paired with @racket['end-work];
@racket['sync], @racket['block], and @racket['touch] are always paired
with @racket['result], @racket['abort], or @racket['suspend]; and
@racket['touch-pause] is always paired with @racket['touch-resume].

In process 0, some event pairs can be nested within other event pairs:
@racket['sync], @racket['block], or @racket['touch] with
@racket['result] or @racket['abort]; and @racket['touch-pause] with
@racket['touch-resume].

An @racket[block] in process 0 is generated when an unsafe operation 
is handled.  This type of event will contain a symbol in the 
@racket[unsafe-op-name] field that is the name of the operation.  In all 
other cases, this field contains @racket[#f].}

@; ----------------------------------------------------------------------

@close-eval[future-eval]
