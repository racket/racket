#lang racket/base
(require (for-syntax racket/base) "utils.rkt" racket/unit)

(provide cond-contracted)

(define-signature-form (cond-contracted stx)
  (syntax-case stx ()
    [(_ nm cnt)
     (if enable-contracts?
         (list #'[contracted (nm cnt)])
         (list #'nm))]))

