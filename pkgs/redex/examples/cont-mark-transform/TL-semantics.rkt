#lang racket

(require "TL-syntax.rkt"
         "SL-syntax.rkt"
         "SL-semantics.rkt"
         redex)

(provide -->TL)

(define -->TL
  (extend-reduction-relation
   -->SL SL #:domain (Σ / (side-condition e_1 (TL-expr? (term e_1))))
   (--> any any
        (side-condition #f)
        "*")
   (--> any any
        (side-condition #f)
        "#")))
(define TL-expr? (redex-match TL e))
