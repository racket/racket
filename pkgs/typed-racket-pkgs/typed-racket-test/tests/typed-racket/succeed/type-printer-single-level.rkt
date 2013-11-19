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
(tr-eval '(:type (Number -> Integer)))

;; if #:verbose, make sure it's the full type
(tr-eval '(:type #:verbose Bar))

(check-equal? (get-output-string out)
              (string-append "(U Integer String)\n[can expand further: Integer]"
                             "(Foo -> Foo)\n[can expand further: Foo]"
                             "(Number -> Integer)\n[can expand further: Integer Number]"
                             "((U String 0 1 Byte-Larger-Than-One Positive-Index-Not-Byte "
                             "Positive-Fixnum-Not-Index Negative-Fixnum "
                             "Positive-Integer-Not-Fixnum Negative-Integer-Not-Fixnum) "
                             "-> (U String 0 1 Byte-Larger-Than-One Positive-Index-Not-Byte "
                             "Positive-Fixnum-Not-Index Negative-Fixnum "
                             "Positive-Integer-Not-Fixnum Negative-Integer-Not-Fixnum"
                             "))\n"))

