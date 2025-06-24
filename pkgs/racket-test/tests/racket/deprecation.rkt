#lang racket/base

(require (for-syntax racket/base
                     racket/deprecation/transformer)
         racket/deprecation
         rackunit
         syntax/parse/define)


(test-case "define-deprecated-alias"

  (test-case "deprecated alias of a constant"
    (define a 42)
    (define-deprecated-alias b a)
    (check-equal? b 42))

  (test-case "deprecated alias of a function"
    (define (f x y)
      (+ x y))
    (define-deprecated-alias g f)
    (check-equal? (g 1 2) 3))

  (test-case "deprecated alias of a macro"
    (define-syntax-parse-rule (m a b c)
      (list a b c))
    (define-deprecated-alias m2 m)
    (check-equal? (m2 1 2 3) (list 1 2 3)))

  (define-syntax-parse-rule (is-deprecated? id:id)
      #:do [(define-values (transformer _)
              (syntax-local-value/immediate #'id (λ () (values #false #false))))]
      #:with result #`'#,(deprecated-alias? transformer)
      result)
  
  (test-case "deprecated alias can be inspected at compile time"
    (define a 42)
    (define-deprecated-alias b a)
    (check-false (is-deprecated? a))
    (check-true (is-deprecated? b)))

  (test-case "deprecated alias provided by module can be inspected at compile time"
    (local-require 'foo)
    (check-false (is-deprecated? a))
    (check-true (is-deprecated? b))))


(module foo racket/base
  (require racket/deprecation)
  (provide a b)
  (define a 42)
  (define-deprecated-alias b a))
