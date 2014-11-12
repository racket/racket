#lang racket/base

;; Test for PR 14829. Make sure the type is not printed
;; for expressions that return Bottom.

(require rackunit
         racket/sandbox)

(define out (open-output-string))

(define tr-eval
  (parameterize ([sandbox-output out])
    (call-with-trusted-sandbox-configuration
     (λ () (make-evaluator 'typed/racket)))))

(with-handlers ([exn? values])
  (tr-eval '(error "foo")))

(check-equal? "" (get-output-string out))
