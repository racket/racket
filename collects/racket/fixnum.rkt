#lang scheme/base
(require '#%flfxnum
         "private/vector-wraps.rkt")

(provide fx->fl fl->fx
         fxabs
         fx+ fx- fx*
         fxquotient fxremainder fxmodulo 
         fxand fxior fxxor
         fxnot fxrshift fxlshift
         fx>= fx> fx= fx< fx<=
         fxmin fxmax
         fxvector? fxvector make-fxvector 
         shared-fxvector make-shared-fxvector
         fxvector-length fxvector-ref fxvector-set!
         fxvector-copy
         in-fxvector for/fxvector for*/fxvector)

(define-vector-wraps "fxvector"
  fxvector? fxvector-length fxvector-ref fxvector-set! make-fxvector
  in-fxvector*
  in-fxvector
  for/fxvector
  for*/fxvector
  fxvector-copy)