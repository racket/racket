#lang scribble/doc

@(require scribble/manual
          (for-label racket/base))

@title{Profile: Statistical Profiler}

The @racketmodname[profile] collection implements a statistical
profiler.  The profiling is done by running a background thread that
collects stack snapshots via @racket[continuation-mark-set->context],
meaning that the result is an estimate of the execution costs and it
is limited to the kind of information that
@racket[continuation-mark-set->context] produces (most notably being
limited to functions calls, and subject to compiler optimizations);
but the result is often useful.  In practice, since this method does
not require recompilation of your source and has very little runtime
overhead, it can be used for longer runs which compensates for these
limits.

@table-of-contents[]

@include-section["toplevel.scrbl"]
@include-section["sampler.scrbl"]
@include-section["analyzer.scrbl"]
@include-section["renderers.scrbl"]
