#lang scribble/doc
@(require "common.rkt"
          (for-label mzlib/cml))

@mzlib[#:mode title cml]

The @racketmodname[mzlib/cml] library defines a number of procedures
that wrap Racket concurrency procedures. The wrapper procedures
have names and interfaces that more closely match those of Concurrent
ML @cite["Reppy99"].


@defproc[(spawn [thunk (-> any)]) thread?]{

Equivalent to @racket[(thread/suspend-to-kill thunk)].}


@defproc[(channel) channel?]{

Equivalent to @racket[(make-channel)].}


@defproc[(channel-recv-evt [ch channel?]) evt?]{

Equivalent to @racket[ch].}


@defproc[(channel-send-evt [ch channel?][v any/c]) evt?]{

Equivalent to @racket[(channel-put-evt ch v)].}


@defproc[(thread-done-evt [thd thread?]) any]{

Equivalent to @racket[(thread-dead-evt thread)].}


@defproc[(current-time) real?]{

Equivalent to @racket[(current-inexact-milliseconds)].}


@defproc[(time-evt [tm real?]) evt?]{

Equivalent to @racket[(alarm-evt tm)].}
