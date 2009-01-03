#lang scheme/base

;; PLT Scheme pre-requisites for any phase

(require (for-syntax scheme/base))

(provide
 (rename-out [datum #%datum])
 (rename-out [#%plain-app #%app])
 #%top #%top-interaction)

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

