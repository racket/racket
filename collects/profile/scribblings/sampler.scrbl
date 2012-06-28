#lang scribble/doc

@(require scribble/manual
          (for-label racket/base racket/contract profile/sampler profile/analyzer))

@title[#:tag "sampler"]{Collecting Profile Information}

@defmodule[profile/sampler]

@defproc[(create-sampler [to-track (or/c thread? custodian?
                                         (listof (or/c thread? custodian?)))]
                         [delay (>=/c 0.0)]
                         [super-cust custodian? (current-custodian)])
         ((symbol?) (any/c) . ->* . any/c)]{

Creates a stack-snapshot collector thread, which tracks the given
@racket[to-track] value every @racket[delay] seconds.  The
@racket[to-track] value can be either a thread (track just that
thread), a custodian (track all threads managed by the custodian), or
a list of threads and/or custodians.  If a custodian is given, it must
be subordinate to @racket[super-cust], which defaults to the current
custodian.

The resulting value is a controller function, which consumes a message
consisting of a symbol and an optional argument, and can affect the
sampler.  The following messages are currently supported:
@itemize[

@item{@racket['pause] and @racket['resume] will stop or resume
  snapshot collection.  These messages can be nested.  Note that the
  thread will continue running---it will just stop collecting
  snapshots.}

@item{@racket['stop] kills the sampler thread.  It should be called
  when no additional data should be collected.  (This is currently
  irreversible: there is no message to start a new sampler thread.)}

@item{@racket['set-tracked!] with a value will change the tracked
  object(s) which were initially specified as the @racket[to-track]
  argument.}

@item{@racket['set-tracked!] with a numeric value will change the
  delay that the sampler is taking between snapshots.  Note that
  although changing this means that the snapshots are not uniformly
  distributed, the results will still be correct: the cpu time between
  samples is taken into account when the collected data is analyzed.}

@item{Finally, a @racket['get-snapshots] message will make the
  controller return the currently collected data.  Note that this can
  be called multiple times, each call will return the data that is
  collected up to that point in time.  In addition, it can be (and
  usually is) called after the sampler was stopped.

  The value that is returned should be considered as an undocumented
  internal detail of the profiler, intended to be sent to
  @racket[analyze-samples] for analysis.  The reason this is not done
  automatically, is that a future extension might allow you to combine
  several sampler results, making it possible to combine a profile
  analysis from several individual runs, possibly from different
  machines.}

]}
