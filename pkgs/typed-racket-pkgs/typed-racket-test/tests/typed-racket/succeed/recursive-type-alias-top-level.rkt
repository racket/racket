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

;; Make sure that recursive type aliases work at the top-level for
;; `make-predicate` and `cast`.

(evaluator '(define-type MyList (U Null (Pairof String MyList))))
(evaluator '((make-predicate MyList) '("a")))
(evaluator '(cast '("a" "b" "c") MyList))
