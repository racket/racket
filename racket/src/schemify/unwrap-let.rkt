#lang racket/base
(require "match.rkt")

(provide unwrap-let)

;; Unwrap `let-values` and friends to reveal the expression
;; Only deal with pre-schemified forms
(define (unwrap-let v #:keep-unsafe-begin? [keep-unsafe-begin? #f])
  (let unwrap-let ([v v])
    (match v
      [`(let-values () ,body) (unwrap-let body)]
      [`(letrec-values () ,body) (unwrap-let body)]
      [`(begin ,body) (unwrap-let body)]
      [`(begin-unsafe ,body) (if keep-unsafe-begin?
                                 v
                                 (unwrap-let body))]
      [`(begin0 ,body) (unwrap-let body)]
      [`,_ v])))
