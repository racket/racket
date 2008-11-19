#lang typed-scheme

(define-syntax-rule (dt nm t)
  (begin (define-type-alias nm t) (provide nm)))

(define-syntax-rule (require/typed/provide lib [nm t] ...)
  (begin
    (require/typed lib [nm t] ...)
    (provide nm ...)))

(provide dt require/typed/provide)
