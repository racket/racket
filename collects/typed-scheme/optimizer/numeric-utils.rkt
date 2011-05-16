#lang racket/base

(require syntax/parse
         "../utils/utils.rkt"
         (types numeric-tower)
         (optimizer utils))

(provide (all-defined-out))

;; layer predicates
;; useful in some cases where subtyping won't do
(define (in-integer-layer? t)
  (subtypeof? t -Int))
(define (in-rational-layer? t)
  (and (subtypeof? t -Rat)
       (not (subtypeof? t -Int))))
(define (in-float-layer? t)
  (subtypeof? t -Flonum))
(define (in-real-layer? t)
  (and (subtypeof? t -Real)
       (not (subtypeof? t -Rat))
       (not (subtypeof? t -Flonum))))
