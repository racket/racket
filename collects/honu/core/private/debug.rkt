#lang racket/base

(require (for-syntax racket/base))

(provide debug)

(define-for-syntax verbose? (getenv "HONU_DEBUG"))
(define-syntax (debug stx)
  (if verbose?
    (syntax-case stx ()
      [(_ str x ...)
       (with-syntax ([file (syntax-source #'str)]
                     [line (syntax-line #'str)]
                     [column (syntax-column #'str)])
         #'(printf (string-append "~a at ~a:~a " str) file line column x ...))])
    #'(void)))

