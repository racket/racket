#;
(exn-pred #rx"does not match the given number:")
#lang typed/racket

;; Check bad arity for recursive invocation of Foo

(define-type (Foo A) (Listof (Foo A A)))

