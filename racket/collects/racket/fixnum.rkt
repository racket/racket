#lang racket/base

(require '#%flfxnum
         "private/vector-wraps.rkt"
         "private/fixnum.rkt"
         "unsafe/ops.rkt"
         (for-syntax racket/base))

(provide fx->fl fl->fx
         fxabs
         fx+ fx- fx*
         fx+/wraparound fx-/wraparound fx*/wraparound
         fxquotient fxremainder fxmodulo 
         fxand fxior fxxor
         fxnot fxrshift fxlshift fxlshift/wraparound fxrshift/logical
         fxpopcount fxpopcount32 fxpopcount16
         fx>= fx> fx= fx< fx<=
         fxmin fxmax
         fixnum-for-every-system?
         fxvector? fxvector make-fxvector 
         shared-fxvector make-shared-fxvector
         fxvector-length fxvector-ref fxvector-set!
         fxvector-copy
         in-fxvector for/fxvector for*/fxvector
         most-positive-fixnum
         most-negative-fixnum)

(define-vector-wraps "fxvector"
  "fixnum?" fixnum?
  fxvector? fxvector-length fxvector-ref fxvector-set! make-fxvector
  unsafe-fxvector-ref unsafe-fxvector-set! unsafe-fxvector-length
  in-fxvector*
  in-fxvector
  for/fxvector
  for*/fxvector
  fxvector-copy
  0
  check-fxvector)
