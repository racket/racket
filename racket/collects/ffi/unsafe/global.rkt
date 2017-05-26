#lang racket/base
(require (only-in '#%unsafe
                  unsafe-register-process-global))

(provide register-process-global)

(define (register-process-global bstr val)
  (unsafe-register-process-global bstr val))
