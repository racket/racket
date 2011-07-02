#lang scribble/doc

@(require scribble/manual "shared.rkt"
          (for-label racket teachpack/htdp/guess-gui))

@teachpack["guess-gui"]{Guess GUI}

@defmodule[#:require-form beginner-require htdp/guess-gui]

The teachpack provides three functions:

@defproc[(control [index natural-number?]) symbol?]{
 reads out the @racket[index]th guess choice, starting with 0}

@defproc[(view [msg (or/c string? symbol?)]) true/c]{
 displays its @racket[msg] argument in the message panel}

@defproc[(connect [handler (-> button% event% true/c)]) true/c]{
 connects a controller (@racket[handler]) with the Check button displays frame}

Example:
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(connect (lambda (e b)
           (begin
             (printf "0th digit: ~s~n" (control 0))
             (view (control 0)))))
))
@;%
