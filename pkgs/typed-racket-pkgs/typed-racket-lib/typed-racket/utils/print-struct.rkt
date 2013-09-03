#lang racket/base

(require (for-syntax racket/base syntax/parse))
(provide define-struct/printer)

(define-syntax (define-struct/printer stx)
  (syntax-parse stx
    [(form name (flds ...) printer:expr)
     #`(define-struct name (flds ...)
         #:property prop:custom-print-quotable 'never
         ;; Eta expansion so that printer is not evaluated
         ;; until needed.
         #:methods gen:custom-write
         [(define (write-proc v port write?)
            (printer v port write?))]
         #:transparent)]))
