#lang racket

;; Make sure that the type printer expands only a single
;; level for (:type ...)

(require rackunit
         racket/sandbox)

(define out (open-output-string))

(define tr-eval
  (parameterize ([sandbox-output out])
    (call-with-trusted-sandbox-configuration
     (thunk (make-evaluator 'typed/racket)))))

(tr-eval '(require typed/racket))
(tr-eval '(define-type Foo (U String Integer)))
(tr-eval '(define-type Bar (Foo -> Foo)))

(tr-eval '(:type Foo))
(tr-eval '(:type Bar))

(check-equal? (get-output-string out) "(U Integer String)\n(Foo -> Foo)\n")
