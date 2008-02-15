#lang scheme/base

(require (for-syntax scheme/base)
         (prefix-in r5rs: r5rs))

(provide (rename-out [datum #%datum]
                     [r5rs:define define]
                     [r5rs:lambda lambda])
         #%app)

;; ----------------------------------------
;; Datum

(define-syntax (datum stx)
  (syntax-case stx ()
    [(_ . thing)
     (if (vector? (syntax-e #'thing))
         (raise-syntax-error 'r6rs
                             "a vector is not an expression"
                             #'thing)
         #`(quote thing))]))

