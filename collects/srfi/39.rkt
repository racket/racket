;; Supported by core PLT, with a slight difference in how a guard is used:
#lang scheme/base

(define make-parameter*
  (let ([make-parameter
         (case-lambda
          [(v) (make-parameter v)]
          [(v guard) (make-parameter (if (and (procedure? guard)
                                              (procedure-arity-includes? guard 1))
                                         ;; apply guard to initial value:
                                         (guard v)
                                         ;; let `make-parameter' complain:
                                         v)
                                     guard)])])
    make-parameter))

(provide (rename-out [make-parameter* make-parameter])
         parameterize)
