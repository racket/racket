#lang racket/base
(require "match.rkt")

;; Recognize functions whose result is always a core procedure of a
;; known arity

(provide constructed-procedure-arity-mask)

(define (constructed-procedure-arity-mask v)
  (match v
    [`(make-struct-field-accessor . ,_)
     1]
    [`(make-struct-field-mutator . ,_)
     2]
    [`,_ #f]))
