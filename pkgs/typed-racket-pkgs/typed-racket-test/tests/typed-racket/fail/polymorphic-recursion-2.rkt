#;
(exn-pred #rx"cannot be applied at a different type")
#lang typed/racket

;; Polymorphic recursion should fail

(define-type (Foo A) (Listof (Foo (Listof A))))

