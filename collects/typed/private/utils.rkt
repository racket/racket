#lang typed/racket/base

(define-syntax-rule (dt nm t)
  (begin (define-type-alias nm t) (provide nm)))

(define-syntax-rule (require/typed/provide lib [nm t] ...)
  (begin
    (require/typed lib [nm t] ...)
    (provide nm ...)))

(define-syntax require-typed-struct/provide
  (syntax-rules ()
    [(_ (nm par) . rest)
     (begin (require-typed-struct (nm par) . rest)
            (provide (struct-out nm)))]
    [(_ nm . rest)
     (begin (require-typed-struct nm . rest)
            (provide (struct-out nm)))]))

(provide dt require/typed/provide require-typed-struct/provide)
