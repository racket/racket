#lang racket/base

(provide unsafe-vector-ref-id)

(define unsafe-vector-ref-id
  (cond
    [(eq? (system-type 'vm) 'chez-scheme)
     ;; Using `unsafe-vector*-ref` is worthwhile,
     ;; because it saves significant compiler effort
     'unsafe-vector*-ref]
    [else
     ;; Using an unsafe operation doesn't work with
     ;; bytecode loading in no-unsafe-operation mode
     'vector*-ref]))
