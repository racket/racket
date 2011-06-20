#lang scribble/doc

@(require scribble/manual "shared.rkt" (for-label scheme teachpack/htdp/lkup-gui))

@teachpack["lkup-gui"]{Lookup GUI}

@defmodule[#:require-form beginner-require htdp/lkup-gui]

The teachpack provides three functions:

@defproc[(control [index natural-number?]) symbol?]{
 reads out the @scheme[index]th guess choice, starting with 0}

@defproc[(view [msg (or/c string? symbol?)]) true/c]{
 displays its @scheme[msg] argument in the message panel}

@defproc[(connect [event-handler (-> button% event% true/c)]) true/c]{
 connects a controller (@scheme[handler]) with the Check button displays frame}

Example:
@;%
@(begin
#reader scribble/comment-reader
(schemeblock
(connect
 (lambda (e b)
   (view (control))))
))
@;%
This example simply mirrors what the user types in to the message field.
