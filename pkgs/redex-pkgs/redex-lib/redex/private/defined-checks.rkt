#lang racket/base

(require "error.rkt"
         racket/undefined)
(provide check-defined-lexical
         check-defined-module)

(define (check-defined-lexical value name desc)
  ;; Needed?
  (when (eq? undefined value)
    (report-undefined name desc)))

(define (check-defined-module thunk name desc)
  (with-handlers ([exn:fail:contract:variable?
                   (λ (_) (report-undefined name desc))])
    (thunk)))

(define (report-undefined name desc)
  (redex-error #f "reference to ~a ~s before its definition" desc name))
