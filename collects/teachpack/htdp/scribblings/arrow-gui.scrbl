#lang scribble/doc

@(require scribble/manual "shared.rkt"
          (for-label scheme teachpack/htdp/arrow-gui))

@teachpack["arrow-gui"]{An Arrow GUI}

@defmodule[#:require-form beginner-require htdp/arrow-gui]

The teachpack provides functions for creating and manipulating an arrow
GUI. We recommend using @racketmodname[2htdp/universe] instead.

@deftech{modelT} @racket[(-> button% event% true)]

A @tech{modelT} is a function that accepts and ignores two arguments. 

@defproc[(control) symbol?]{Reads out the current state of the message
field.} 

@defproc[(view [s (or/c string? symbol?)]) true]{Displays @racket[s] in the
message field.} 

@defproc[(connect [l (unsyntax @tech{modelT})][r (unsyntax @tech{modelT})][u (unsyntax @tech{modelT})][d (unsyntax @tech{modelT})]) true]{Connects four
controllers with the four directions in the arrow window.}

Example:
@(begin
#reader scribble/comment-reader
(racketblock
;; Advanced
(define (make-model dir)
  (lambda (b e)
    (begin
      (view dir)
      (printf "~a ~n" (control)))))

(connect (make-model "left")
         (make-model "right")
         (make-model "up")
         (make-model "down"))
))
Now click on the four arrows. The message field contains the current
direction, the print-out the prior contents of the message field.
