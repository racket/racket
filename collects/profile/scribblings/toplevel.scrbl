#lang scribble/doc

@(require scribble/manual
          (for-label scheme profile profile/sampler
                     (only-in profile/analyzer analyze-samples)
                     (prefix-in text: profile/render-text)))

@title{Toplevel Interface}

@defmodule[profile]

This module provides one procedure and one macro that are convenient
high-level entry points for timing expressions.  This hides the
details that are available through other parts of the library, and is
intended as a convenient tool for profiling code.

@defproc[(profile-thunk
          [thunk (-> any/c)]
          [#:delay   delay      nonnegative-number?        0.05]
          [#:repeat  iterations exact-nonnegative-integer? 1]
          [#:threads threads?   any/c                      #f]
          [#:render  renderer   (profile? . -> . any/c)    text:render]
          [#:periodic-renderer periodic-renderer
           (or/c #f (list/c nonnegative-number? (profile? . -> . any/c)))
           #f])
         void?]{

Executes the given thunk while collecting profiling data, and render
this data when done.  Keyword arguments can customize the profiling:
@itemize[

@item{The profiler works by @scheme[create-sampler] starting a
  ``sampler'' thread whose job is to collect stack samples
  periodically (using @scheme[continuation-mark-set->context]).
  @scheme[delay] determines the amount of time the sampler
  @scheme[sleep]s for between samples.  Note that this is will be
  close, but not identical to, the frequency in which data is actually
  sampled.}

@item{When the profiled computation takes a short amount of time, the
  collected data will not be accurate.  In this case, you can specify
  an @scheme[iterations] argument to repeat the evaluation a number of
  times which will improve the accuracy of the resulting report.}

@item{Normally, the sampler collects snapshots of the
  @scheme[current-thread]'s stack.  If there is some computation that
  happens on a different thread, that work will not be reflected in
  the results: the only effect will be suspiciously small value for
  the observed time, because the collected data is taking into account
  the cpu time that the thread actually performed (it uses
  @scheme[current-process-milliseconds] with the running thread as an
  argument).  Specifying a non-@scheme[#f] value for the
  @scheme[threads?] argument will arrange for all threads that are
  started during the evaluation to be tracked.  Note that this means
  that the computation will actually run in a new sub-custodian, as
  this is the only way to be able to track such threads.}

@item{Once the computation has finished, the sampler is stopped, and
  the accumulated data is collected.  It is then analyzed by
  @scheme[analyze-samples], and the analyzed profile data is fed into
  a renderer.  Use an identity function (@scheme[values]) to get the
  analyzed result, and render it yourself, or use one of the existing
  renderers (see @secref["renderers"]).}

@item{The @scheme[periodic-renderer] argument can be set to a list
  holding a delay time and a renderer.  In this case, the given
  renderer will be called periodically.  This is useful for cases
  where you want a dynamically updated display of the results.  This
  delay should be larger than the sampler delay.}

]}

@defform[(profile expr keyword-arguments ...)]{

A macro version of @scheme[profile-thunk].  The keyword arguments can
be specified in the same was as for a function call: they can appear
before and/or after the expression to be profiled.}
