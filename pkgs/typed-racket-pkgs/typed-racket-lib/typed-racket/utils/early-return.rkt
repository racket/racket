#lang racket/base

(require (for-syntax racket/base syntax/parse))
(provide early-return)

(define-syntax (early-return stx)
  (syntax-parse stx
    [(_ e:expr ... #:return-when e0:expr e1:expr rest ...)
     (syntax/loc stx
       (let ()
         e ...
         (if e0 e1
             (early-return rest ...))))]
    [(_ e:expr ... #:return-unless e0:expr e1:expr rest ...)
     (syntax/loc stx
       (let ()
         e ...
         (if e0
             (early-return rest ...)
             e1)))]
    [(_ e:expr ...) (syntax/loc stx (let () e ...))]))



