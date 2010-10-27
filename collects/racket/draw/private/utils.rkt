#lang racket/base
(require ffi/unsafe)

(provide define-mz
         define-enum
         define/provide)

(define-syntax-rule (define-mz id type)
  (define id (get-ffi-obj 'id #f type)))

(define-syntax define-enum
  (syntax-rules ()
    [(_ n) (begin)]
    [(_ n id . ids) (begin
                      (define id n)
                      (provide id)
                      (define-enum (+ n 1) . ids))]))

(define-syntax-rule (define/provide id val)
  (begin
    (define id val)
    (provide id)))
