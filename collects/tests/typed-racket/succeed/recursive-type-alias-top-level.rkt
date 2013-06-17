#lang racket

;; Make sure type aliases are registered from a module
;; to another context appropriately

(require racket/sandbox)

(define evaluator
  (call-with-trusted-sandbox-configuration
   (λ () (make-evaluator 'typed/racket))))

(evaluator '(require typed/racket))
(evaluator '(module a typed/racket
              (define-type (Foo A) (Option (Listof (Foo A))))
              (: x (Foo Integer))
              (define x #f)
              (provide x)))
(evaluator '(require 'a))
(evaluator 'x)

