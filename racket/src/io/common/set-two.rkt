#lang racket/base

(provide bytes-set-two!)

(define big-endian? (system-big-endian?))

(define (bytes-set-two! out-bstr j hi lo)
  (cond
    [big-endian?
     (bytes-set! out-bstr j hi)
     (bytes-set! out-bstr (+ j 1) lo)]
    [else
     (bytes-set! out-bstr j lo)
     (bytes-set! out-bstr (+ j 1) hi)]))
