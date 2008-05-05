#lang scribble/doc

@(require scribble/manual
          (for-label scheme 
	  	     teachpack/htdp/arrow-gui))

@title[#:tag "arrow-gui"]{An Arrow GUI: arrow-gui.ss}

@declare-exporting[teachpack/htdp/arrow-gui]

The teachpack provides operations for creating and manipulating an arrow
GUI. We recommend using the @seclink["world"]{world teachpack} instead.

@deftech{modelT} @scheme[(-> button% event% true)] 

A @tech{modelT} is a function that accepts and ignores two arguments. 

@defproc[(control) symbol?]{Reads out the current state of the message
field.} 

@defproc[(view [s (or/c string? symbol?)]) true]{Displays @scheme[s] in the
message field.} 

@defproc[(connect [l (unsyntax @tech{modelT})][r (unsyntax @tech{modelT})][u (unsyntax @tech{modelT})][d (unsyntax @tech{modelT})]) true]{Connects four
controllers with the four directions in the arrow window.}

Example:
@(begin
#reader scribble/comment-reader
(schemeblock
;; Advanced 
(define (make-model dir)
   (lambda (b e)
     (begin
       (view dir)
       (printf "~a ~n" (control)))))

(connect
   (make-model "left")
   (make-model "right")
   (make-model "up")
   (make-model "down"))
))
Now click on the four arrows. The message field contains the current
direction, the print-out the prior contents of the message field.
