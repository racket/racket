#lang scribble/doc

@(require scribble/manual
          (for-label racket/base racket/contract profile profile/sampler
                     errortrace/errortrace-lib
                     (only-in profile/analyzer analyze-samples profile?)
                     (prefix-in text: profile/render-text)))

@title{Toplevel Interface}

@defmodule[profile]

This module provides one procedure and one macro that are convenient
high-level entry points for profiling expressions.  It abstracts over
details that are available through other parts of the library, and is
intended as a convenient tool for profiling code.

@defproc[(profile-thunk
          [thunk (-> any/c)]
          [#:delay   delay      (>=/c 0.0)        0.05]
          [#:repeat  iterations exact-nonnegative-integer? 1]
          [#:threads threads?   any/c                      #f]
          [#:render  renderer   (profile? . -> . any/c)    text:render]
          [#:periodic-renderer periodic-renderer
           (or/c #f (list/c (>=/c 0.0)
                            (profile? . -> . any/c)))
           #f]
          [#:use-errortrace? use-errortrace? any/c #f])
         void?]{

Executes the given @racket[thunk] and collect profiling data during
execution, eventually analyzing and rendering this.  Keyword arguments
can customize the profiling:
@itemize[

@item{The profiler works by starting a ``sampler'' thread to
  periodically collect stack snapshots (using
  @racket[continuation-mark-set->context]).  To determine the
  frequency of these collections, the sampler thread sleeps
  @racket[delay] seconds between collections.  Note that this is will
  be close, but not identical to, the frequency in which data is
  actually sampled.  (The @racket[delay] value is passed on to
  @racket[create-sampler], which creates the sampler thread.)}
 
@item{When @racket[use-errortrace?] is not @racket[#f], more accurate stack
  snapshots are captured using @seclink["top" #:doc '(lib
  "errortrace/scribblings/errortrace.scrbl")]{Errortrace}. Note that when this
  is provided, it will only profile uncompiled files and files compiled while
  using @racket[errortrace-compile-handler], and the profiled program must be
  run using @commandline{racket -l errortrace -t program.rkt} Removing compiled
  files (with extension @tt{.zo}) is sufficient to enable this.}

@item{Due to the statistical nature of the profiler, longer executions
  result in more accurate analysis.  You can specify a number of
  @racket[iterations] to repeat the @racket[thunk] to collect more
  data.}

@item{Normally, the sampler collects only snapshots of the
  @racket[current-thread]'s stack.  Profiling a computation that
  creates threads will therefore lead to bad analysis: the timing
  results will be correct, but because the profiler is unaware of
  other threads the observed time will be suspiciously small, and work
  done in other threads will not be included in the results.  To track
  all threads, specify a non-@racket[#f] value for the
  @racket[threads?] argument---this will execute the computation in a
  fresh custodian, and keep track of all threads under this
  custodian.}

@item{Once the computation is done and the sampler is stopped, the
  accumulated data is analyzed (by @racket[analyze-samples]) and the
  resulting profile value is sent to the @racket[renderer] function.
  See @secref["renderers"] for available renderers.  You can also use
  @racket[values] as a ``renderer''---in this case
  @racket[profile-thunk] returns the analyzed information which can
  now be rendered multiple times, or saved for rendering directly
  using one of the renderers, perhaps multiple times for different
  views.}

@item{To provide feedback information during execution, specify a
  @racket[periodic-renderer].  This should be a list holding a delay
  time (in seconds) and a renderer function.  The delay determines the
  frequency in which the renderer is called, and it should be larger
  than the sampler delay (usually much larger since it can involve
  more noticeable overhead, and it is intended for a human observer).}

]}

@defform[(profile expr keyword-arguments ...)]{

A macro version of @racket[profile-thunk].  Keyword arguments can be
specified as in a function call: they can appear before and/or after
the expression to be profiled.}
