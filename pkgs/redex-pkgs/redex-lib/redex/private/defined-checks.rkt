#lang racket/base

(require "error.rkt")
(provide check-defined-module)

(define (check-defined-module thunk name desc)
  (with-handlers ([exn:fail:contract:variable?
                   (λ (_) (report-undefined name desc))])
    (thunk)))

(define (report-undefined name desc)
  (raise
   (exn:fail:contract:variable
    (format "~s: undefined;\n cannot use ~a before its definition" name desc)
    (current-continuation-marks)
    name)))
