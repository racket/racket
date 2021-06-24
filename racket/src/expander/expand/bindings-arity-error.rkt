#lang racket/base
(require "../syntax/scope.rkt")

(provide raise-bindings-arity-error)

(define (raise-bindings-arity-error who ids vals)
  (apply raise-result-arity-error
         who
         (length ids)
         (cond
           [(null? ids) ""]
           [else (format "\n  in: definition of ~a~a" (syntax-e (car ids)) (if (pair? (cdr ids)) " ..." ""))])
         vals))
