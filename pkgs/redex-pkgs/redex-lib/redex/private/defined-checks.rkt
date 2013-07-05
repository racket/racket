#lang racket/base

(require "error.rkt")
(provide check-defined-lexical
         check-defined-module)

(define (check-defined-lexical value name desc)
  (when (eq? (letrec ([x x]) x) value)
    (report-undefined name desc)))

(define (check-defined-module thunk name desc)
  (with-handlers ([exn:fail:contract:variable?
                   (λ (_) (report-undefined name desc))])
    (thunk)))

(define (report-undefined name desc)
  (redex-error #f "reference to ~a ~s before its definition" desc name))
