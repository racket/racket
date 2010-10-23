#lang racket

; things to be shared among all sections of the style guide

(require (for-label racket)
         scribble/manual)

(provide (for-label (all-from-out racket))
         (all-from-out scribble/manual))


; (provide good)
