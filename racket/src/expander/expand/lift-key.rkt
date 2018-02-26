#lang racket/base

;; A lift key represents a target for lifting, such as a particular
;; module body, a particular namespace, or a particular capture point

(provide generate-lift-key)

(define (generate-lift-key)
  (gensym 'lift))
