#lang racket/base
(require (only-in '#%unsafe
                  unsafe-register-process-global
                  unsafe-get-place-table))

(provide register-process-global
         get-place-table)

(define (register-process-global bstr val)
  (unsafe-register-process-global bstr val))

(define (get-place-table)
  (unsafe-get-place-table))
