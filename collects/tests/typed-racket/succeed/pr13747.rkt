#lang racket

;; Test that `require/typed` works at the top-level

(require racket/sandbox)

(define evaluator
  (call-with-trusted-sandbox-configuration
   (λ () (make-evaluator 'typed/racket))))

(evaluator '(require/typed racket/base [values (Integer -> Integer)]))
(evaluator '(values 1))

