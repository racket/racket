#lang scheme/base
(require scheme/unit)
(provide (all-defined-out))

(define-signature dmap^
  (dmap-meet))

(define-signature promote-demote^
  (var-promote var-demote))

(define-signature constraints^
  (exn:infer?
   fail-sym
   ;; inference failure - masked before it gets to the user program
   (define-syntaxes (fail!)
     (syntax-rules ()
       [(_ s t) (raise fail-sym)]))
   cset-meet cset-meet*
   no-constraint
   empty-cset
   insert
   cset-combine
   c-meet))

(define-signature restrict^
  (restrict))

(define-signature infer^
  (infer infer/vararg infer/dots))
