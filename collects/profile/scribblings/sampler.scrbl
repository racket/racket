#lang scribble/doc

@(require scribble/manual
          (for-label scheme profile/sampler profile/analyzer))

@title[#:tag "sampler"]{Collecting Profile Information}

@defmodule[profile/sampler]

@defproc[(create-sampler [to-track (or/c thread? custodian?
                                         (listof (or/c thread? custodian?)))]
                         [delay nonnegative-number?]
                         [super-cust custodian? (current-custodian)])
         ((symbol?) (any/c) . ->* . any/c)]{

Creates a sample collector thread, which tracks the given
@scheme[to-track] value every @scheme[delay] seconds.  The
@scheme[to-track] value can be either a thread (track just that
thread), a custodian (track all threads managed by the custodian), or
a list of threads and/or custodians.  If a custodian is given, it must
be subordinate to @scheme[super-cust], which defaults to the current
custodian.

The resulting value is a controller function, which consumes a message
consisting of a symbol and an optional argument, and can affect the
sampler.  The following messages are currently supported:
@itemize[

@item{@scheme['pause] and @scheme['resume] will stop or resume data
collection.  These messages can be nested.  Note that the thread will
continue running it will just stop collecting snapshots.}

@item{@scheme['stop] kills the controlled thread.  It should be called
  when no additional data should be collected.  (This is currently
  irreversible: there is no message to start a new sampler thread.)}

@item{@scheme['set-tracked!] with a value will change the tracked
  objects (initially specified as the @scheme[to-track] argument) to
  the given value.}

@item{@scheme['set-tracked!] with a value will change the delay that
  the sampler us taking between snapshots.  Note that although
  changing this means that the snapshots are not uniformly
  distributed, the results will still be sensible --- this is because
  the cpu time between samples is taken into account when the
  resulting data is analyzed.}

@item{Finally, a @scheme['get-snapshots] message will make the
  controller return the currently collected data.  Note that this can
  be called multiple times, each call will return the data thatis
  collected up to that point in time.  In addition, it can be (and
  usually is) called after the sampler was stopped.

  The value that is returned should be considered as an undocumented
  internal detail of the profiler, to be sent to
  @scheme[analyze-samples] for analysis.  The reason this is not done
  automatically, is that a future extension might allow you to combine
  several sampler results, making it possible to combine a profile
  analysis from several individual runs, possibly from different
  machines.}

]}
