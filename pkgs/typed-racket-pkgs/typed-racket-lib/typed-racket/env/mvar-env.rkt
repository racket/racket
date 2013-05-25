#lang racket/base

(require syntax/id-table racket/dict)

(provide mvar-env register-mutated-var is-var-mutated?)

(define mvar-env (make-free-id-table))

(define (register-mutated-var id)
  (dict-set! mvar-env id #t))

(define (is-var-mutated? id)
  (dict-ref mvar-env id #f))
