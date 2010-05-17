#lang scribble/doc
@(require "common.rkt"
          (for-label mzlib/cml))

@mzlib[#:mode title cml]

The @schememodname[mzlib/cml] library defines a number of procedures
that wrap PLT Scheme concurrency procedures. The wrapper procedures
have names and interfaces that more closely match those of Concurrent
ML @cite["Reppy99"].


@defproc[(spawn [thunk (-> any)]) thread?]{

Equivalent to @scheme[(thread/suspend-to-kill thunk)].}


@defproc[(channel) channel?]{

Equivalent to @scheme[(make-channel)].}


@defproc[(channel-recv-evt [ch channel?]) evt?]{

Equivalent to @scheme[ch].}


@defproc[(channel-send-evt [ch channel?][v any/c]) evt?]{

Equivalent to @scheme[(channel-put-evt ch v)].}


@defproc[(thread-done-evt [thd thread?]) any]{

Equivalent to @scheme[(thread-dead-evt thread)].}


@defproc[(current-time) real?]{

Equivalent to @scheme[(current-inexact-milliseconds)].}


@defproc[(time-evt [tm real?]) evt?]{

Equivalent to @scheme[(alarm-evt tm)].}
