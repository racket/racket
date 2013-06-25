#lang racket/base

;; Test that struct: and define-struct: work at the
;; top-level.
;;
;; Test for PR 11669

(require rackunit
         racket/sandbox)

(define out (open-output-string))

(define tr-eval
  (parameterize ([sandbox-output out])
    (call-with-trusted-sandbox-configuration
     (λ () (make-evaluator 'typed/racket)))))

(check-not-exn
  (λ ()
    (tr-eval '(struct: Foo ([x : Integer])))
    (tr-eval '(define-struct: Bar ([y : Foo])))
    (tr-eval '(define-type Qux (U String Integer)))
    (tr-eval '(struct: Quux ([qux : Qux])))
    (tr-eval 'Quux-qux)
    (tr-eval 'Foo)
    (tr-eval 'make-Bar)))

